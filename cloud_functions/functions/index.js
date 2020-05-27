const functions = require('firebase-functions');

const admin = require('firebase-admin');
admin.initializeApp();

const accountSid = 'AC430b7a42761407d169e6190524ca2c3b';
const authToken = '31df65c4ae26170be592ef0a71621d65';
const client = require('twilio')(accountSid, authToken);

exports.newSubscriber = functions.firestore
    .document('/subscribers/{phoneNumber}')
    .onCreate((snap, context) => {
        console.log('New subscriber', context.params.phoneNumber);

        return client.messages
            .create({
                body: 'Welcome to Coronalert! 🚨\nI will send you an alert every day with the latest COVID data from the places you are subscribed to.\nFeel free to subscribe to more places at:\nwww.coronalert.live',
                from: '+12173949203',
                to: '+12172205771'
            })
            .then(message => console.log(message.sid));
    });

exports.newSubscription = functions.firestore
    .document('/subscribers/{phoneNumber}')
    .onUpdate((change, context) => {
        const newValue = change.after.data();
        const latest = newValue.latest;

        if (latest === "counties") {
            console.log('New county',
                context.params.phoneNumber,
                newValue.counties.pop(),
            );
        } else if (latest === "states") {
            console.log('New state',
                context.params.phoneNumber,
                newValue.states.pop(),
            );
        } else if (latest === "countries") {
            console.log('New country',
                context.params.phoneNumber,
                newValue.countries.pop(),
            );
        }
    });

// exports.addMessage = functions.https.onRequest(async (req, res) => {
//     const original = req.query.text;
//     const writeResult = await admin.firestore().collection('messages').add({ original: original });
//     res.json({ result: `Message with ID: ${writeResult.id} added.` });
// });

// exports.makeUppercase = functions.firestore.document('/messages/{documentId}')
//     .onCreate((snap, context) => {
//         const original = snap.data().original;

//         console.log('Uppercasing', context.params.documentId, original);

//         const uppercase = original.toUpperCase();

//         return snap.ref.set({ uppercase }, { merge: true });
//     });

// exports.helloWorld = functions.https.onRequest((request, response) => {
//  response.send("Hello from Firebase!");
// });
