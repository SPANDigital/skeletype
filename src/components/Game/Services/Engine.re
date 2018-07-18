module Constructs = {
  type lane =
    | Top
    | Middle
    | Bottom;

  type skeleton = {
    startTime: int,
    lane,
    intervalId: ref(option(float)),
    mutable stopTime: int,
    mutable status: Skeleton.status,
  };

  type word = {
    mutable randomWord: string,
    wordLane: lane,
  };
};

module Handlers = {
  let laneToInt = (lane: Constructs.lane) =>
    switch (lane) {
    | Top => 1
    | Middle => 2
    | Bottom => 3
    };

  let fillWords =
    Constructs.(
      () => {
        let wordTop: word = {randomWord: "hello", wordLane: Top};
        let wordMiddle: word = {randomWord: "react", wordLane: Middle};
        let wordBottom: word = {randomWord: "cpt", wordLane: Bottom};
        [|wordTop, wordMiddle, wordBottom|];
      }
    );
};

[@bs.val] external setTimeout : (unit => unit, int) => float = "setTimeout";
[@bs.val] external clearTimeout : float => unit = "clearTimeout";
[@bs.val] external setInterval : (unit => unit, int) => float = "setInterval";
[@bs.val] external clearInterval : float => unit = "clearInterval";

[@bs.module] external heartIcon : string = "../../assets/8-bit-heart.png";
[@bs.module]
external skeletonIcon : string = "../../assets/undead/undead_idle.gif";