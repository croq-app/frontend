importScripts(
  'https://storage.googleapis.com/workbox-cdn/releases/6.4.1/workbox-sw.js'
);
let daysInSeconds = 60 * 60 * 24;

workbox.routing.registerRoute(
  () => true,
  new workbox.strategies.CacheFirst({
    cacheName: "croq",
    plugins: [
      // new workbox.expiration.Plugin({
      //   maxAgeSeconds: 28 * daysInSeconds,
      //   purgeOnQuotaError: true,
      // })
    ]
  }),
);