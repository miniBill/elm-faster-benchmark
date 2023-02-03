{
  function getWorkersCount() {
    try {
      if (navigator.hardwareConcurrency) return navigator.hardwareConcurrency;
    } catch {}
    return 2;
  }

  const workersCount = getWorkersCount();

  var pool = Array();

  for (let index = 0; index < workersCount; index++) {
    pool.push(new Worker("./backend-js.js"));
  }

  // @ts-ignore
  const Frontend = Elm.Frontend;

  const app = Frontend.init({
    node: document.getElementsByTagName("main")[0],
    flags: { workersCount: workersCount },
  });

  app.ports.toBackend.subscribe(function ({ index, value }) {
    pool[index].postMessage(value);
  });

  pool.forEach((worker) => {
    worker.onmessage = function ({ data }) {
      app.ports.fromBackend.send(data);
    };
  });
}
