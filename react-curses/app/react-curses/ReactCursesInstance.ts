import type { BaseComponentViewConfig } from "./ReactCursesViewConfigRegistry.ts";

export type TextInstance = number;

export default class Instance {
  nativeTag: number;
  children: (Instance | TextInstance)[];
  viewConfig: BaseComponentViewConfig;

  constructor(tag: number, viewConfig: BaseComponentViewConfig) {
    this.nativeTag = tag;
    this.children = [];
    this.viewConfig = viewConfig;
  }
}
