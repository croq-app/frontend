import { Elm } from '../Main.elm';
import PortFunnelGeolocation from './PortFunnel/Geolocation.js';

const PortFunnel = require('./PortFunnel.js');

// Start the Elm application.
let app = Elm.Main.init({
    node: document.querySelector('main'),
});

PortFunnel.subscribe(app, { modules: ["Geolocation "] });


// Start service workers
const enableServiceWorkers = false;
(async (enable) => {
    if (enable && "serviceWorker" in navigator) {
        // App shell SW 
        try {
            const registration = await navigator.serviceWorker.register("/sw.js", { scope: "/" });
            if (registration.installing) {
                console.log("App shell SW installing");
            } else if (registration.waiting) {
                console.log("App shell SW installed");
            } else if (registration.active) {
                console.log("App shell SW active");
            }
        } catch (error) {
            console.error(`Registration failed with ${error}`);
        }

        // API shell SW 
        try {
            const registration = await navigator.serviceWorker.register("/sw-api.js", { scope: "/" });
            if (registration.installing) {
                console.log("API SW installing");
            } else if (registration.waiting) {
                console.log("API SW installed");
            } else if (registration.active) {
                console.log("API SW active");
            }
        } catch (error) {
            console.error(`Registration failed with ${error}`);
        }
    }
})(enableServiceWorkers);