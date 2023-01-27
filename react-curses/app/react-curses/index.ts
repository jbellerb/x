import ReactReconciler from "https://esm.sh/react-reconciler@0.26.1?dev";
import * as HostConfig from "./ReactCursesHostConfig.ts";
import { Text, View } from "./NativeComponents.tsx";

const roots = new Map();
const ReactReconcilerInst = ReactReconciler(HostConfig);

export { Text, View };

export function render(
  element: any,
  callback?: () => void | null | undefined,
) {
  let root = ReactReconcilerInst.createContainer(
    0,
    0,
    false,
    null,
  );

  return ReactReconcilerInst.updateContainer(
    element,
    root,
    null,
    callback || (() => null),
  );
}

export default {
  render,
  Text,
  View,
};
