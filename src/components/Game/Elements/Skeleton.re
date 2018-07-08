type status =
  | Walking
  | Attacking
  | Dying
  | Dead;

let rowComponent = ReasonReact.statelessComponent("Skeleton");
let make = (~time, ~startTime, ~deathTime, ~lane, ~status, _children) => {
  ...rowComponent,
  render: self => {
    let startPosition = time - startTime;
    let timeString = ref(string_of_int(startPosition * 2) ++ "px");
    let positionFromTop =
      switch (lane) {
      | 1 => "16%"
      | 2 => "44%"
      | 3 => "72%"
      };

    if (status === Dying || status === Dead) {
      Js.log(deathTime);
      timeString := string_of_int(deathTime * 2) ++ "px";
    };

    <div
      className=(
        Cn.make([
          "skeleton",
          "dying" |> Cn.ifTrue(status === Dying),
          "dead" |> Cn.ifTrue(status === Dead),
        ])
      )
      style=(
        ReactDOMRe.Style.make(
          ~top=positionFromTop,
          ~transform={j|translateX($timeString)|j},
          ignore(),
        )
      )
    />;
  },
};