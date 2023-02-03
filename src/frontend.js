{
  function getWorkersCount() {
    try {
      if (navigator.hardwareConcurrency) return navigator.hardwareConcurrency;
    } catch { }
    return 2;
  }

  const workersCount = getWorkersCount();

  /** @type {Worker[]} */
  var pool = Array();

  function getWorker(index) {
    var worker;
    if (index in pool) {
      worker = pool[index];
    } else {
      worker = pool[index] = new Worker("./backend-js.js");
      worker.onmessage = function ({ data }) {
        app.ports.fromBackend.send(data);
      };
    }
    return worker;
  }

  // @ts-ignore
  const Frontend = Elm.Frontend;

  const app = Frontend.init({
    node: document.getElementsByTagName("main")[0],
    flags: { workersCount: workersCount },
  });

  app.ports.toBackend.subscribe(function ({ index, value }) {
    /** @type {Worker} */
    var worker = getWorker(index);
    worker.postMessage(value);
  });

  app.ports.terminate.subscribe(function (index) {
    if (index in pool) {
      pool[index].terminate()
      delete pool[index]
    }
  })
}
