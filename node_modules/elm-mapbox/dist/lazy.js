window.customElements.define(
  "elm-mapbox-map",
  class MapboxMap extends HTMLElement {
    constructor() {
      super();
      this._eventRegistrationQueue = {};
    }

    addEventListener(type, fn, ...args) {
      this._eventRegistrationQueue[type] =
        this._eventRegistrationQueue[type] || [];
      return this._eventRegistrationQueue[type].push(fn);
    }

    removeEventListener(type, fn, ...args) {
      const queue = this._eventRegistrationQueue[type] || [];
      const index = queue.findIndex(fn);
      if (index >= 0) {
        queue.splice(index, 1);
      }
      return;
    }
  }
);
