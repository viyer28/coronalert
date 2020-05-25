import { registerCustomElement, registerPorts } from "elm-mapbox";
import "mapbox-gl/dist/mapbox-gl.css";
import { Elm } from "./src/Main.elm";
const firebase = require("firebase/app");
require("firebase/firestore");

const token = "pk.eyJ1Ijoic3BvdHRpeWVyIiwiYSI6ImNqZmQyZnVkejIwbGgyd29iZnR3bGVvMXUifQ.fVrLRiLoyIoPfAGm5ozmMg";

registerCustomElement({ token: token });
var app = Elm.Main.init({ node: document.body });
registerPorts(app);

firebase.initializeApp({
  apiKey: 'AIzaSyD4rtRUXeVy8jXJzVR7PKP986sunn7jef4',
  authDomain: 'coronalert-a5911.firebaseapp.com',
  projectId: 'coronalert-a5911'
});

var db = firebase.firestore()

// ports
app.ports.firebaseWrite.subscribe(function (data) {
  var collection = firebase.firestore().collection('subscribers');

  var phoneRef = collection.doc(data.phoneNumber);
  if (data.id == "counties") {
    phoneRef.set({
      counties: firebase.firestore.FieldValue.arrayUnion({
        county: data.county,
        province: data.province,
      }),
    }, { merge: true });
  } else if (data.id == "states") {
    phoneRef.set({
      states: firebase.firestore.FieldValue.arrayUnion({
        country: data.country,
        state: data.province,
      }),
    }, { merge: true });
  } else if (data.id == "countries") {
    phoneRef.set({
      countries: firebase.firestore.FieldValue.arrayUnion({
        country: data.country,
        state: data.province,
      }),
    }, { merge: true });
  }

  // var existsQuery = collection.where("phoneNumber", "==", data.phoneNumber);

  // existsQuery.get()
  //   .then(function (querySnapshot) {
  //     if (querySnapshot.size == 0) {
  //       console.log("No document exists!");

  //       if (data.id == "counties") {
  //         collection.add({
  //           phoneNumber: data.phoneNumber,
  //           counties: [{
  //             county: data.county,
  //             province: data.province,
  //           }],
  //           states: [],
  //           countries: [],
  //         });
  //       } else if (data.id == "states") {
  //         collection.add({
  //           phoneNumber: data.phoneNumber,
  //           counties: [],
  //           states: [{
  //             country: data.country,
  //             state: data.province,
  //           }],
  //           countries: [],
  //         });
  //       } else if (data.id == "countries") {
  //         collection.add({
  //           phoneNumber: data.phoneNumber,
  //           counties: [],
  //           states: [],
  //           countries: [{
  //             country: data.country,
  //             province: data.province,
  //           }],
  //         });
  //       }

  //     } else {
  //       querySnapshot.forEach(function (doc) {
  //         console.log(doc.id, " => ", doc.data());
  //       });
  //     }
  //   })
  //   .catch(function (error) {
  //     console.log("Error getting documents: ", error);
  //   });
});