const functions = require('firebase-functions');

const admin = require('firebase-admin');
admin.initializeApp();

const accountSid = functions.config().twilio.sid;
const authToken = functions.config().twilio.token;
const twilio = require('twilio')(accountSid, authToken);

const axios = require('axios');
const db = admin.firestore();
var _ = require('lodash');

// newSubscriber: sends a messagee to a new subscriber welcoming them to Coronalert (using Twilio)
exports.newSubscriber = functions.firestore
    .document('/subscribers/{phoneNumber}')
    .onCreate(async (snap, context) => {
        console.log('New subscriber', context.params.phoneNumber);
        const data = snap.data();
        const latest = data.latest;
        var firstSub = '';

        if (latest === "counties") {
            let sub = data.counties.pop();
            firstSub = '\nYou are now subscribed to:\n✅ ' + sub.county + ' County\n\n';
        } else if (latest === "states") {
            let sub = data.states.pop();
            firstSub = '\nYou are now subscribed to:\n✅ ' + sub.state + '\n\n';
        } else if (latest === "countries") {
            let sub = data.countries.pop();
            if (sub.state === null || sub.state === "null") {
                firstSub = '\nYou are now subscribed to:\n✅ ' + sub.country + '\n\n';
            } else {
                firstSub = '\nYou are now subscribed to:\n✅ ' + sub.state + ', ' + sub.state + '\n\n';
            }
        }

        db.collection("subscribers").doc(context.params.phoneNumber).set({
            premium: false,
            sentAlerts: 0,
            latest: "new",
        }, { merge: true });

        const message = await twilio.messages
            .create({
                body: 'Welcome to Coronalert! 🚨\n\nI will send you daily alerts with the latest COVID data from the places you are subscribed to.\nJust text me "STOP" and I will stop sending you alerts!\n' + firstSub + 'Subscribe to more places at:\nwww.coronalert.live',
                from: '+12173949203',
                to: '+1' + context.params.phoneNumber
            });
        return console.log(message.sid);
    });

// newSubscription: sends a message to a subscriber when he/she subscribes to a new place (using Twilio)
exports.newSubscription = functions.firestore
    .document('/subscribers/{phoneNumber}')
    .onUpdate(async (change, context) => {
        const newValue = change.after.data();
        const latest = newValue.latest;
        var newSub = '';

        if (latest === "counties") {
            let sub = newValue.counties.pop();
            newSub = '\nYou are now subscribed to:\n✅ ' + sub.county + ' County\n\n';
        } else if (latest === "states") {
            let sub = newValue.states.pop();
            newSub = '\nYou are now subscribed to:\n✅ ' + sub.state + '\n\n';
        } else if (latest === "countries") {
            let sub = newValue.countries.pop();
            if (sub.state === null || sub.state === "null") {
                newSub = '\nYou are now subscribed to:\n✅ ' + sub.country + '\n\n';
            } else {
                newSub = '\nYou are now subscribed to:\n✅ ' + sub.state + ', ' + sub.country + '\n\n';
            }
        } else if (latest === "premium") {
            newSub = '\nWelcome to Coronalert Premium ❤️\n\n'
        } else if (latest === "counter") {
            if (newValue.sentAlerts === 5 && newValue.premium === false) {
                newSub = '\nYour free subscription is expiring in 2 days!\nIf you would like to keep receiving alerts, upgrade to Coronalert Premium:\nwww.coronalert.live/premium\n\n';
            } else if (newValue.sentAlerts === 6 && newValue.premium === false) {
                newSub = '\nYour free subscription is expiring in 1 day!\nIf you would like to keep receiving alerts, upgrade to Coronalert Premium:\nwww.coronalert.live/premium\n\n';
            } else if (newValue.sentAlerts === 7 && newValue.premium === false) {
                newSub = '\nYour free subscription has expired!\nIf you would like to keep receiving alerts, upgrade to Coronalert Premium:\nwww.coronalert.live/premium\n\n';
            } else {
                return null;
            }
        } else {
            return null;
        }

        const message = await twilio.messages
            .create({
                body: 'Thanks for subscribing! 🙏\n' + newSub + 'Subscribe to more places at:\nwww.coronalert.live',
                from: '+12173949203',
                to: '+1' + context.params.phoneNumber
            });
        return console.log(message.sid);
    });

// scheduledAlert: sends alert messages to all valid subscribers at 10am CT 
exports.scheduledAlert =
    functions.pubsub
        .schedule('every day 10:00')
        .timeZone('America/Chicago')
        .onRun((context) => {
            return axios.get('https://disease.sh/v2/jhucsse/counties')
                .then(response_one => {
                    const county_data = response_one.data;
                    return axios.get('https://disease.sh/v2/jhucsse/')
                        .then(response_two => {
                            const country_state_data = response_two.data;
                            return getValidReceivers().then(result => {
                                return result.forEach(doc => {
                                    var msg = 'Good morning ☀️\nHere\'s your daily Coronalert 🚨\n\n'

                                    if (doc.data().counties !== undefined) {
                                        for (county of doc.data().counties) {
                                            let c = county_data.find(x =>
                                                x.county === county.county &&
                                                x.province === county.province
                                            );
                                            msg += c.county + ' County, ' + c.province + '\n';
                                            msg += '🤒 ' + (c.stats.confirmed).toString() + ' confirmed\n';
                                            msg += '💀 ' + (c.stats.deaths).toString() + ' deaths\n\n';
                                        }
                                    }

                                    if (doc.data().states !== undefined) {
                                        for (state of doc.data().states) {
                                            let s = country_state_data.find(x =>
                                                x.country === state.country &&
                                                x.province === state.state
                                            );
                                            msg += s.province + '\n';
                                            msg += '🤒 ' + (s.stats.confirmed).toString() + ' confirmed\n';
                                            msg += '💀 ' + (s.stats.deaths).toString() + ' deaths\n\n';
                                        }
                                    }

                                    if (doc.data().countries !== undefined) {
                                        for (country of doc.data().countries) {
                                            if (country.state === "null") {
                                                let c = country_state_data.find(x =>
                                                    x.country === country.country
                                                );
                                                msg += c.country + '\n';
                                                msg += '🤒 ' + (c.stats.confirmed).toString() + ' confirmed\n';
                                                msg += '💀 ' + (c.stats.deaths).toString() + ' deaths\n\n';
                                            } else {
                                                let c = country_state_data.find(x =>
                                                    x.country === country.country &&
                                                    x.province === country.state
                                                );
                                                msg += c.province + ', ' + c.country + '\n';
                                                msg += '🤒 ' + (c.stats.confirmed).toString() + ' confirmed\n';
                                                msg += '💀 ' + (c.stats.deaths).toString() + ' deaths\n\n';
                                            }
                                        }
                                    }

                                    msg += 'Stay alert, stay safe!\nSubscribe to more places at:\nwww.coronalert.live';

                                    db.collection("subscribers").doc(doc.id).set({
                                        sentAlerts: doc.data().sentAlerts + 1,
                                        latest: 'counter'
                                    }, { merge: true });

                                    const message = twilio.messages
                                        .create({
                                            body: msg,
                                            from: '+12173949203',
                                            to: '+1' + doc.id
                                        });
                                    return console.log(message.sid);
                                });
                            });
                        })
                        .catch(error => {
                            console.log(error);
                        });
                })
                .catch(error => {
                    console.log(error);
                });
        });

// getValidReceivers: determines which subscribers can receive alerts, i.e. premium subscribers and free subscribers who have been subscribed for less than a week
async function getValidReceivers() {
    const subRef = db.collection("subscribers");
    const isPremium = subRef.where('premium', '==', true).get();
    const isValidFree = subRef
        .where('premium', '==', false)
        .where('sentAlerts', '<', 7).get();

    const [
        isPremiumQuerySnapshot,
        isValidFreeQuerySnapshot
    ] = await Promise.all([isPremium, isValidFree]);

    const isPremiumArray = isPremiumQuerySnapshot.docs;
    const isValidFreeArray = isValidFreeQuerySnapshot.docs;

    return _.concat(isPremiumArray, isValidFreeArray);
}