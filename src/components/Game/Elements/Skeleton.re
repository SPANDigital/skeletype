type status =
  | Walking
  | Attacking
  | Dying;

let rowComponent = ReasonReact.statelessComponent("Skeleton");
let make = (~startTime, ~deathTime, ~time, ~lane, ~status, _children) => {
  ...rowComponent,
  render: self => {
    let startPosition = time - startTime;
    let timeString = ref(string_of_int(startPosition * 2) ++ "px");
    let positionFromTop = string_of_int(lane * 30) ++ "%";

    if (status === Dying) {
      Js.log(deathTime);
      timeString := string_of_int(deathTime * 2) ++ "px";
      ();
    };

    <div
      className=(
        Cn.make(["skeleton", "dying" |> Cn.ifTrue(status === Dying)])
      )
      style=(
        ReactDOMRe.Style.make(
          ~top=positionFromTop,
          ~transform={j|translateX($timeString)|j},
          (),
        )
      )
    />;
  },
};