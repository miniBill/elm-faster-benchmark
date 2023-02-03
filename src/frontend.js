{
  function getWorkersCount() {
    try {
      if (navigator.hardwareConcurrency) return navigator.hardwareConcurrency;
    } catch {}
    return 2;
  }

  const workersCount = getWorkersCount();

  var pool = Array();

  // @ts-ignore
  const Frontend = Elm.Frontend;

  const app = Frontend.init({
    node: document.getElementsByTagName("main")[0],
    flags: { workersCount: workersCount },
  });

  app.ports.toBackend.subscribe(function ({ index, value }) {
    var worker;
    if (index in pool) {
      worker = pool[index];
    } else {
      worker = pool[index] = new Worker("./backend-js.js");
      worker.onmessage = function ({ data }) {
        app.ports.fromBackend.send(data);
      };
    }
    pool[index].postMessage(value);
  });
}
