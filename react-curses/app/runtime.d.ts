declare namespace NativeBridge {
  enum LogLevel {
    Error = "ERROR",
    Warn = "WARN",
    Info = "INFO",
    Debug = "DEBUG",
    Trace = "TRACE",
  }

  function log(level: LogLevel, message: string): void;
  function error(message: string): void;
  function warn(message: string): void;
  function info(message: string): void;
  function debug(message: string): void;
  function trace(message: string): void;

  function createView(
    reactTag: number,
    viewName: string,
    rootTag: number,
    props: Object,
  ): void;
  function setChildren(parentTag: number, reactTags: number[]): void;
  function manageChildren(
    parentTag: number,
    moveFromIndices: number[],
    moveToIndices: number[],
    addChildReactTags: number[],
    addAtIndices: number[],
    removeAtIndices: number[],
  ): void;
  function updateView(
    reactTag: number,
    viewName: string,
    props: Object,
  ): void;
}
