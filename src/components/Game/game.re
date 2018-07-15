[%bs.raw {|require('./game.css')|}];
[@bs.val] external setTimeout : (unit => unit, int) => float = "setTimeout";
[@bs.val] external clearTimeout : float => unit = "clearTimeout";
[@bs.val] external setInterval : (unit => unit, int) => float = "setInterval";
[@bs.val] external clearInterval : float => unit = "clearInterval";

[@bs.module] external heartIcon : string = "../../assets/8-bit-heart.png";
[@bs.module]
external skeletonIcon : string = "../../assets/undead/undead_idle.gif";

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

type state = {
  time: int,
  score: int,
  killed: int,
  lives: int,
  input: string,
  countdown: int,
  words: array(word),
  skeletons: array(skeleton),
  intervalId: ref(option(float)),
  refTextField: ref(option(Dom.element)),
};

type gameState =
  | Menu
  | CountDown
  | Playing
  | Won
  | Lost;

type action =
  | Tick
  | ClearInput
  | Countdown
  | StartGame
  | DamagePlayer
  | ProcessInput(string)
  | AddWord(string, lane)
  | ReplaceWord(string, string)
  | SpawnSkeleton(int, lane)
  | KillSkeleton(int, lane)
  | FlagSkeletonAsAttacking(skeleton)
  | FlagSkeletonAsDead(lane)
  | RemoveSkeleton(lane)
  | SetSkeletonInterval(skeleton, option(float));

let laneToInt = (lane: lane) =>
  switch (lane) {
  | Top => 1
  | Middle => 2
  | Bottom => 3
  };

let setSectionRef = (theRef, {ReasonReact.state}) =>
  state.refTextField := Js.Nullable.toOption(theRef);

let fillWords = () => {
  let wordTop: word = {randomWord: "yo", wordLane: Top};
  let wordMiddle: word = {randomWord: "hey", wordLane: Middle};
  let wordBottom: word = {randomWord: "wow", wordLane: Bottom};
  [|wordTop, wordMiddle, wordBottom|];
};

let gameComponent = ReasonReact.reducerComponent("Game");
let make = _children => {
  let click = (event, self) =>
    self.ReasonReact.send(KillSkeleton(self.state.time, Top));
  let handleType = (event, self) => {
    let text = ReactDOMRe.domElementToObj(
                 ReactEventRe.Keyboard.target(event),
               )##value;
    self.ReasonReact.send(ProcessInput(text));
  };
  {
    ...gameComponent,
    initialState: () => {
      time: 0,
      score: 0,
      killed: 0,
      lives: 10,
      countdown: 1,
      input: "",
      skeletons: [||],
      words: fillWords(),
      intervalId: ref(None),
      refTextField: ref(None),
    },
    didMount: self => {
      let intervalId =
        Some(setInterval(() => self.ReasonReact.send(Countdown), 1000));
      self.ReasonReact.state.intervalId := intervalId;
    },
    willUnmount: self =>
      switch (self.state.intervalId^) {
      | Some(id) => clearInterval(id)
      | None => ()
      },
    didUpdate: ({oldSelf, newSelf}) =>
      switch (newSelf.state.refTextField^) {
      | Some(field) =>
        let node = ReactDOMRe.domElementToObj(field);
        ignore(node##focus());
      | _ => ()
      },
    reducer: (action, state) =>
      switch (action) {
      | StartGame =>
        ReasonReact.SideEffects(
          (
            self => {
              let intervalId =
                Some(setInterval(() => self.ReasonReact.send(Tick), 25));
              self.ReasonReact.state.intervalId := intervalId;
            }
          ),
        )

      | Countdown =>
        ReasonReact.UpdateWithSideEffects(
          {...state, countdown: state.countdown - 1},
          (
            self =>
              if (self.state.countdown === 0) {
                self.send(StartGame);
              }
          ),
        )

      | Tick =>
        ReasonReact.UpdateWithSideEffects(
          {...state, time: state.time + 1},
          (
            self => {
              let skeletonsCurrentlyAttacking =
                Helpers.filter(
                  ~f=x => self.state.time - x.startTime === 400,
                  state.skeletons,
                );

              Array.mapi(
                (i, skeleton) =>
                  self.send(FlagSkeletonAsAttacking(skeleton)),
                skeletonsCurrentlyAttacking,
              );

              /* Every 100ms */
              if (state.time mod 100 === 0) {
                let occupiedLanes =
                  ArrayLabels.map(s => s.lane, state.skeletons);

                let deployedLane = ref(Top);

                if (Array.length(occupiedLanes) !== 3) {
                  if (! Helpers.contains(Top, occupiedLanes)) {
                    deployedLane := Top;
                  };
                  if (! Helpers.contains(Middle, occupiedLanes)) {
                    deployedLane := Middle;
                  };
                  if (! Helpers.contains(Bottom, occupiedLanes)) {
                    deployedLane := Bottom;
                  };
                  self.send(SpawnSkeleton(state.time, deployedLane^));
                };
              };
            }
          ),
        )

      | ProcessInput(input) =>
        ReasonReact.UpdateWithSideEffects(
          {...state, input},
          (
            self =>
              if (Js.String.length(self.state.input) > 0) {
                let matchedWord =
                  Helpers.filter(
                    ~f=x => x.randomWord === self.state.input,
                    state.words,
                  );

                if (Array.length(matchedWord) > 0) {
                  let targetWord = matchedWord[0].randomWord;
                  let targetLane = matchedWord[0].wordLane;

                  self.send(
                    ReplaceWord(targetWord, Dictionary.getRandomWord()),
                  );
                  self.send(KillSkeleton(self.state.time, targetLane));
                  self.send(ClearInput);
                };
              }
          ),
        )

      | ClearInput => ReasonReact.Update({...state, input: ""})

      | AddWord(randomWord, wordLane) =>
        let word: word = {randomWord, wordLane};
        let existingWords = state.words;
        switch (wordLane) {
        | Top => existingWords[0] = word
        | Middle => existingWords[1] = word
        | Bottom => existingWords[2] = word
        };
        ReasonReact.Update({...state, words: existingWords});

      | ReplaceWord(oldWord, newWord) =>
        let updatedWords = state.words;
        if (Array.length(updatedWords) > 0) {
          let entryToReplace =
            Helpers.find(~f=x => x.randomWord === oldWord, updatedWords);
          entryToReplace.randomWord = newWord;
        };
        ReasonReact.Update({
          ...state,
          words: updatedWords,
          score: state.score + 10,
        });

      | SpawnSkeleton(startTime, lane) =>
        let skeleton: skeleton = {
          intervalId: ref(None),
          startTime,
          stopTime: 0,
          lane,
          status: Walking,
        };
        let existingSkeletons = state.skeletons |> ArrayLabels.to_list;
        let updatedSkeletons =
          [skeleton, ...existingSkeletons] |> ArrayLabels.of_list;
        ReasonReact.Update({...state, skeletons: updatedSkeletons});

      | KillSkeleton(stopTime, lane) =>
        let killIt = skeleton => {
          if (skeleton.status === Walking) {
            skeleton.stopTime = state.time - skeleton.startTime;
          };
          skeleton.status = Dying;
          ReasonReact.UpdateWithSideEffects(
            {
              ...state,
              killed: state.killed + 1,
              skeletons: state.skeletons,
              score: state.score + 50,
            },
            (
              self => {
                setTimeout(() => self.send(FlagSkeletonAsDead(lane)), 1000);
                ();
              }
            ),
          );
        };

        switch (Helpers.find(~f=x => x.lane === lane, state.skeletons)) {
        | skeleton => killIt(skeleton)
        | exception Not_found => ReasonReact.NoUpdate
        };

      | FlagSkeletonAsAttacking(skeleton) =>
        skeleton.status = Attacking;
        skeleton.stopTime = state.time - skeleton.startTime;

        ReasonReact.UpdateWithSideEffects(
          {...state, skeletons: state.skeletons},
          (
            self => {
              let intervalId =
                Some(setInterval(() => self.send(DamagePlayer), 1000));
              self.send(SetSkeletonInterval(skeleton, intervalId));
            }
          ),
        );

      | SetSkeletonInterval(skeleton, intervalId) =>
        skeleton.intervalId := intervalId;
        ReasonReact.Update({...state, skeletons: state.skeletons});

      | FlagSkeletonAsDead(lane) =>
        let flagIt = skeleton => {
          skeleton.status = Dead;
          switch (skeleton.intervalId^) {
          | Some(float) => clearInterval(float)
          | None => ()
          };

          ReasonReact.UpdateWithSideEffects(
            {...state, skeletons: state.skeletons},
            (
              self => {
                setTimeout(() => self.send(RemoveSkeleton(lane)), 1000);
                ();
              }
            ),
          );
        };

        switch (Helpers.find(~f=x => x.lane === lane, state.skeletons)) {
        | skeleton => flagIt(skeleton)
        | exception Not_found => ReasonReact.NoUpdate
        };

      | RemoveSkeleton(lane) =>
        let remainingSkeletons =
          Helpers.filter(~f=x => x.lane !== lane, state.skeletons);
        ReasonReact.Update({...state, skeletons: remainingSkeletons});

      | DamagePlayer => ReasonReact.Update({...state, lives: state.lives - 1})
      },
    render: ({state, handle, send}) => {
      let {time, input, lives, skeletons, score, killed, words, _} = state;
      <div className="world">
        <Fog />
        <div className="layout">
          <div
            className=(
              Cn.make([
                "countdown",
                "done" |> Cn.ifTrue(state.countdown <= 0),
              ])
            )>
            (ReasonReact.string(string_of_int(state.countdown)))
          </div>
          <div
            className=(
              Cn.make(["youDied", "show" |> Cn.ifTrue(state.lives <= 0)])
            )>
            (ReasonReact.string("YOU DIED"))
          </div>
          <div className="header">
            <div className="beginning">
              (ReasonReact.string("SKELETYPE"))
            </div>
            <div className="middle">
              <div className="killed">
                <img className="skeletonIcon" src=skeletonIcon />
                (ReasonReact.string("(" ++ string_of_int(killed) ++ ")"))
              </div>
              <div className="time">
                <img className="heartIcon" src=heartIcon />
                (ReasonReact.string("(" ++ string_of_int(lives) ++ ")"))
              </div>
              <div className="score">
                (ReasonReact.string("Score:" ++ string_of_int(score)))
              </div>
            </div>
            <div className="end" />
          </div>
          <div className="menu">
            (
              ReasonReact.array(
                Array.mapi(
                  (i, word) => <Word input randomWord=word.randomWord />,
                  words,
                ),
              )
            )
          </div>
          <div className="content">
            (
              ReasonReact.array(
                Array.mapi(
                  (i, skeleton) =>
                    <Skeleton
                      key={j|skeleton-$i|j}
                      lane=(laneToInt(skeleton.lane))
                      startTime=skeleton.startTime
                      stopTime=skeleton.stopTime
                      status=skeleton.status
                      time
                    />,
                  skeletons,
                ),
              )
            )
            <Row lane="1" />
            <Row lane="2" />
            <Row lane="3" />
          </div>
          <div className="footer">
            <input
              ref=(handle(setSectionRef))
              value=state.input
              onChange=(
                event =>
                  send(
                    ProcessInput(
                      ReactDOMRe.domElementToObj(
                        ReactEventRe.Form.target(event),
                      )##value,
                    ),
                  )
              )
            />
          </div>
        </div>
      </div>;
    },
  };
};