import { Elm } from './src/Main.elm'

Elm.Main.init({
  node: document.getElementById('main'),
});

if ("serviceWorker" in navigator) {
   navigator.serviceWorker.register("service-worker.js");
}
