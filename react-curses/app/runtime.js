export let NativeBridge;

(function () {
  const LogLevel = {
    Error: "ERROR",
    Warn: "WARN",
    Info: "INFO",
    Debug: "DEBUG",
    Trace: "TRACE",
  };

  function log(level, message) {
    console.log(message);
  }

  const error = (message) => log(LogLevel.Error, message);
  const warn = (message) => log(LogLevel.Warn, message);
  const info = (message) => log(LogLevel.Info, message);
  const debug = (message) => log(LogLevel.Debug, message);
  const trace = (message) => log(LogLevel.Trace, message);

  function createView(
    reactTag,
    viewName,
    rootTag,
    props,
  ) {
    const payload = { reactTag, viewName, rootTag, props };
    debug(`createView: ${JSON.stringify(payload)}`);
  }

  function setChildren(parentTag, reactTags) {
    const payload = { parentTag, reactTags };
    debug(`setChildren: ${JSON.stringify(payload)}`);
  }

  function manageChildren(
    parentTag,
    moveFromIndices,
    moveToIndices,
    addChildReactTags,
    addAtIndices,
    removeAtIndices,
  ) {
    const payload = {
      parentTag,
      moveFromIndices,
      moveToIndices,
      addChildReactTags,
      addAtIndices,
      removeAtIndices,
    };
    debug(`manageChildren: ${JSON.stringify(payload)}`);
  }

  function updateView(
    reactTag,
    viewName,
    props,
  ) {
    const payload = { reactTag, viewName, props };
    debug(`updateView: ${JSON.stringify(payload)}`);
  }

  NativeBridge = {
    LogLevel,
    log,
    error,
    warn,
    info,
    debug,
    trace,
    createView,
    setChildren,
    manageChildren,
    updateView,
  };
})();
