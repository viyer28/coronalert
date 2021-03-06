import { registerCustomElement, registerPorts } from "elm-mapbox";
import "mapbox-gl/dist/mapbox-gl.css";
import { Elm } from "./src/Main.elm";
const firebase = require("firebase/app");
require("firebase/firestore");

const token = "pk.eyJ1Ijoic3BvdHRpeWVyIiwiYSI6ImNqZmQyZnVkejIwbGgyd29iZnR3bGVvMXUifQ.fVrLRiLoyIoPfAGm5ozmMg";

var stripe = Stripe('pk_live_p5afHJ20fpbPORRrAdHXRfyq00c6LKjfF3');

// Creates main Elm application
registerCustomElement({ token: token });
var app = Elm.Main.init({
  node: document.body,
  flags: {
    height: window.innerHeight,
    width: window.innerWidth
  }
});
registerPorts(app);

// Initializes Firebase
firebase.initializeApp({
  apiKey: 'AIzaSyD4rtRUXeVy8jXJzVR7PKP986sunn7jef4',
  authDomain: 'coronalert-a5911.firebaseapp.com',
  projectId: 'coronalert-a5911'
});

var db = firebase.firestore()

// PORTS

// fireebaseWrite: adds a new subscription to a subscriber
app.ports.firebaseWrite.subscribe(function (data) {
  var collection = firebase.firestore().collection('subscribers');

  var phoneRef = collection.doc(data.phoneNumber);
  phoneRef.get().then(function (doc) {
    if (doc.exists && doc.data().premium === false) {
      app.ports.invalidSubscription.send(true);
    } else {
      app.ports.invalidSubscription.send(false);
      if (data.id == "counties") {
        phoneRef.set({
          counties: firebase.firestore.FieldValue.arrayUnion({
            county: data.county,
            province: data.province,
          }),
          latest: data.id,
        }, { merge: true });
      } else if (data.id == "states") {
        phoneRef.set({
          states: firebase.firestore.FieldValue.arrayUnion({
            country: data.country,
            state: data.province,
          }),
          latest: data.id,
        }, { merge: true });
      } else if (data.id == "countries") {
        phoneRef.set({
          countries: firebase.firestore.FieldValue.arrayUnion({
            country: data.country,
            state: data.province,
          }),
          latest: data.id,
        }, { merge: true });
      }
    }
  }).catch(function (error) {
    console.log("Error getting document:", error);
  });
});

// processPremium: launches Stripe checkout for Premium
app.ports.processPremium.subscribe(function (data) {
  stripe.redirectToCheckout({
    lineItems: [{
      price: 'price_HNSRH0HxhZfHOO',
      quantity: 1
    }],
    mode: 'payment',
    successUrl: data.url + 'premium/success/' + data.phoneNumber,
    cancelUrl: data.url + 'premium/failure/' + data.phoneNumber
  });
});

// firebaseUpgrade: after successful payment, upgrade account on Firebase
app.ports.firebaseUpgrade.subscribe(function (data) {
  var collection = firebase.firestore().collection('subscribers');

  var phoneRef = collection.doc(data);
  phoneRef.set({
    premium: true,
    latest: "premium",
  }, { merge: true });
});