export interface BaseComponentViewConfig {
  baseModuleName: string;
  uiViewClassName: string;
}

const registry = {
  view: { baseModuleName: "view", uiViewClassName: "Layout" },
  text: { baseModuleName: "text", uiViewClassName: "Paragraph" },
};

export default function (tag: string): BaseComponentViewConfig {
  // @ts-ignore
  return registry[tag];
}
