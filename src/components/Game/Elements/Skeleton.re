[@bs.module]
external gifWalking : string = "../../../assets/undead/undead_walk.gif";
[@bs.module]
external gifDying : string = "../../../assets/undead/undead_death.gif";
[@bs.module]
external gifDead : string = "../../../assets/undead/undead_dead.gif";
[@bs.module]
external gifAttacking : string = "../../../assets/undead/undead_attack.gif";

type status =
  | Walking
  | Attacking
  | Dying
  | Dead;

let statusToString = (status: status) =>
  switch (status) {
  | Walking => "Walking"
  | Attacking => "Attacking"
  | Dying => "Dying"
  | Dead => "Dead"
  };

let rowComponent = ReasonReact.statelessComponent("Skeleton");
let make = (~time, ~startTime, ~stopTime, ~lane, ~status, _children) => {
  ...rowComponent,
  render: self => {
    let progress = time - startTime;
    let divideByLength = (x: float) => x /. float_of_int(430);
    let percentageDone = progress |> float_of_int |> divideByLength;

    let timeString = ref(string_of_int(progress * 2) ++ "px");
    let determineGif = status =>
      switch (status) {
      | Walking => {j|url($gifWalking)|j}
      | Attacking => {j|url($gifAttacking)|j}
      | Dying => {j|url($gifDying)|j}
      | Dead => {j|url($gifDead)|j}
      };

    let positionFromTop =
      switch (lane) {
      | 1 => "16%"
      | 2 => "44%"
      | 3 => "72%"
      };

    /* Stop unit from moving */
    if (status === Dying || status === Dead || status === Attacking) {
      timeString := string_of_int(stopTime * 2) ++ "px";
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
          ~backgroundImage=determineGif(status),
          ~top=positionFromTop,
          ~transform={j|translateX($timeString)|j},
          ignore(),
        )
      )
    />;
  },
};