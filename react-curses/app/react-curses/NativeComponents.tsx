import React from "https://esm.sh/react@17.0.1?dev";

export function Text(props: { children: any }) {
  return (
    <text>
      {props.children}
    </text>
  );
}

export function View(props: { children: any }) {
  return (
    <view>
      {props.children}
    </view>
  );
}
