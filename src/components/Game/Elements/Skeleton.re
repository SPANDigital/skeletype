[@bs.module]
external gifWalking : string = "../../../assets/undead/undead_walk.gif";
[@bs.module]
external gifDying0 : string = "../../../assets/undead/undead_death.gif";
[@bs.module]
external gifDying1 : string = "../../../assets/undead/undead_death_1.gif";
[@bs.module]
external gifDying2 : string = "../../../assets/undead/undead_death_2.gif";

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
let make = (~time, ~startTime, ~stopTime, ~lane, ~status, ~speed, _children) => {
  ...rowComponent,
  render: self => {
    let progress = time - startTime;
    let divideByLength = (x: float) => x /. float_of_int(400);
    let percentageDone = progress |> float_of_int |> divideByLength;

    let timeString = ref(string_of_int(progress * speed) ++ "px");
    let determineGif = status =>
      switch (status) {
      | Walking => {j|url($gifWalking)|j}
      | Attacking => {j|url($gifAttacking)|j}
      | Dying =>
        switch (lane) {
        | 1 => {j|url($gifDying0)|j}
        | 2 => {j|url($gifDying1)|j}
        | 3 => {j|url($gifDying2)|j}
        }
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
      timeString := (string_of_int(stopTime * speed) ++ "px");
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