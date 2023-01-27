/// <reference path="./runtime.d.ts" />

import React from "https://esm.sh/react@17.0.1?dev";
import ReactCurses, { Text, View } from "./react-curses/index.ts";

function App() {
  const [state, setState] = React.useState<number>(0);

  React.useEffect(() => {
    const timeout = setTimeout(() => {
      setState(1);
    }, 1000);

    return () => clearTimeout(timeout);
  }, [setState]);

  return (
    <View>
      <Text>Count: {state}</Text>
    </View>
  );
}

ReactCurses.render(<App />);
