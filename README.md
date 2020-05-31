# Coronalert 🚨
The COVID Map with Text Alerts <br>
[See the live website](https://www.coronalert.live)

## Instructions
Use Parcel to build:
```
npm install -g parcel-bundler
parcel index.html
```
## Structure
The Elm source files are in: <br>
[src/](src/) <br>
<br>
The Javascript ports for dealing with Firebase and Stripe are in: <br>
[index,js](index.js) <br>
<br>
The Firebase cloud functions for sending text alerts are in: <br>
[cloud_functions/functions/index.js](cloud_functions/functions/index.js) <br>
