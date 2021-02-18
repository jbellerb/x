import Reconciler from "https://esm.sh/react-reconciler@0.26.1?dev";
import Instance, { TextInstance } from "./ReactCursesInstance.ts";
import getViewConfigForType from "./ReactCursesViewConfigRegistry.ts";

interface HostContext {}

export const supportsMutation = true;
export const supportsPersistence = false;
export const supportsHydration = false;

let nextTag = 1;
function allocateTag(): number {
  const tag = nextTag;
  nextTag += 1;
  return tag;
}

export function createInstance(
  type: string,
  props: Object,
  rootContainerInstance: number,
  hostContext: HostContext,
  internalInstanceHandle: Object,
): Instance {
  const tag = allocateTag();
  const viewConfig = getViewConfigForType(type);

  NativeBridge.createView(
    tag,
    viewConfig.uiViewClassName,
    rootContainerInstance,
    { ...props, children: undefined },
  );

  return new Instance(tag, viewConfig);
}

export function createTextInstance(
  text: string,
  rootContainerInstance: number,
  hostContext: HostContext,
  internalInstanceHandle: Object,
): TextInstance {
  const tag = allocateTag();

  NativeBridge.createView(
    tag,
    "RawText",
    rootContainerInstance,
    { text },
  );

  return tag;
}

export function appendInitialChild(
  parent: Instance,
  child: Instance | TextInstance,
): void {
  parent.children.push(child);
}

export function finalizeInitialChildren(
  parentInstance: Instance,
  type: string,
  props: Object,
  rootContainerInstance: number,
  hostContext: HostContext,
): boolean {
  if (parentInstance.children.length > 0) {
    const nativeTags = parentInstance.children.map((child) =>
      typeof child === "number" ? child : child.nativeTag
    );

    NativeBridge.setChildren(
      parentInstance.nativeTag,
      nativeTags,
    );
  }

  return false;
}

export function prepareUpdate(
  instance: Instance,
  type: string,
  oldProps: Object,
  newProps: Object,
  rootContainerInstance: number,
  hostContext: HostContext,
): Object | null {
  return true;
}

export function shouldSetTextContent(type: string, props: Object): boolean {
  return false;
}

export function getRootHostContext(
  rootContainerInstance: number,
): HostContext {
  return {};
}

export function getChildHostContext(
  parentHostContext: HostContext,
  type: string,
  rootContainerInstance: number,
): HostContext {
  return {};
}

export function getPublicInstance(
  instance: Instance | TextInstance,
): Instance | TextInstance {
  return instance;
}

export function prepareForCommit(containerInfo: number): Object | null {
  return null;
}

export function resetAfterCommit(containerInfo: number): void {
}

export function preparePortalMount(containerInfo: number): void {
}

export function now(): number {
  return now();
}

export function scheduleTimeout(
  func: (...args: any[]) => void,
  delay: number,
  ...args: any
): number {
  return setTimeout(func, delay, ...args);
}

export function cancelTimeout(id: number): void {
  return clearTimeout(id);
}

export const noTimeout = -1;

export function queueMicrotask(
  fn: () => void,
): void {
  throw new Error("Not implemented.");
}

export const isPrimaryRenderer = true;

export function appendChild(
  parentInstance: Instance,
  child: Instance | TextInstance,
): void {
  const index = parentInstance.children.indexOf(child);

  if (index >= 0) {
    parentInstance.children.splice(index, 1);
    parentInstance.children.push(child);

    NativeBridge.manageChildren(
      parentInstance.nativeTag,
      [index],
      [parentInstance.children.length - 1],
      [],
      [],
      [],
    );
  } else {
    parentInstance.children.push(child);

    NativeBridge.manageChildren(
      parentInstance.nativeTag,
      [],
      [],
      [typeof child === "number" ? child : child.nativeTag],
      [parentInstance.children.length - 1],
      [],
    );
  }
}

export function appendChildToContainer(
  parentInstance: number,
  child: Instance | TextInstance,
): void {
  NativeBridge.setChildren(parentInstance, [
    typeof child === "number" ? child : child.nativeTag,
  ]);
}

export function insertBefore(
  parentInstance: Instance,
  child: Instance | TextInstance,
  beforeChild: Instance | TextInstance,
): void {
  const index = parentInstance.children.indexOf(child);

  if (index >= 0) {
    parentInstance.children.splice(index, 1);
    const beforeChildIndex = parentInstance.children.indexOf(beforeChild);
    parentInstance.children.splice(beforeChildIndex, 0, child);

    NativeBridge.manageChildren(
      parentInstance.nativeTag,
      [index],
      [beforeChildIndex],
      [],
      [],
      [],
    );
  } else {
    const beforeChildIndex = parentInstance.children.indexOf(beforeChild);
    parentInstance.children.splice(beforeChildIndex, 0, child);

    NativeBridge.manageChildren(
      parentInstance.nativeTag,
      [],
      [],
      [typeof child === "number" ? child : child.nativeTag],
      [beforeChildIndex],
      [],
    );
  }
}

export function insertInContainerBefore(
  parentInstance: number,
  child: Instance | TextInstance,
  beforeChild: Instance | TextInstance,
): void {
  throw new Error("Not implemented.");
}

export function removeChild(
  parentInstance: Instance,
  child: Instance | TextInstance,
): void {
  const index = parentInstance.children.indexOf(child);

  parentInstance.children.splice(index, 1);

  NativeBridge.manageChildren(
    parentInstance.nativeTag,
    [],
    [],
    [],
    [],
    [index],
  );
}

export function removeChildFromContainer(
  parentInstance: number,
  child: Instance | TextInstance,
): void {
  NativeBridge.manageChildren(
    parentInstance,
    [],
    [],
    [],
    [],
    [0],
  );
}

export function resetTextContent(instance: Instance): void {
  throw new Error("Not implemented.");
}

export function commitTextUpdate(
  textInstance: TextInstance,
  oldText: string,
  newText: string,
): void {
  NativeBridge.updateView(
    textInstance,
    "RawText",
    { text: newText },
  );
}

export function commitMount(
  instance: Instance,
  type: string,
  newProps: Object,
  internalInstanceHandle: Object,
): void {
}

export function commitUpdate(
  instance: Instance,
  updatePayloadTODO: Object,
  type: string,
  oldProps: Object,
  newProps: Object,
  internalInstanceHandle: Object,
): void {
  const viewConfig = instance.viewConfig;

  NativeBridge.updateView(
    instance.nativeTag,
    viewConfig.uiViewClassName,
    newProps,
  );
}

export function hideInstance(instance: Instance): void {
  throw new Error("Not implemented.");
}

export function hideTextInstance(textInstance: TextInstance): void {
  throw new Error("Not implemented.");
}

export function unhideInstance(instance: Instance): void {
  throw new Error("Not implemented.");
}

export function unhideTextInstance(
  textInstance: TextInstance,
): void {
  throw new Error("Not implemented.");
}

export function clearContainer(container: number): void {
}

export function getInstanceFromNode(node: any) {
  throw new Error("Not implemented.");
}

export function shouldDeprioritizeSubtree(): boolean {
  throw new Error("Not implemented.");
}

export function scheduleDeferredCallback() {
  throw new Error("Not implemented.");
}

export function cancelDeferredCallback() {
  throw new Error("Not implemented.");
}
