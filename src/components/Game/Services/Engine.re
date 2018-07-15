/* [@bs.val] external setTimeout : (unit => unit, int) => float = "setTimeout";
   [@bs.val] external clearTimeout : float => unit = "clearTimeout";
   [@bs.val] external setInterval : (unit => unit, int) => float = "setInterval";
   [@bs.val] external clearInterval : float => unit = "clearInterval";

   let doCountDown = () =>
     ReasonReact.UpdateWithSideEffects(
       {...state, countdown: state.countdown - 1},
       self =>
         if (self.state.countdown === 0) {
           self.send(StartGame);
         },
     );

   let tick = () =>
     ReasonReact.UpdateWithSideEffects(
       {...state, time: state.time + 1},
       self => {
         let skeletonsCurrentlyAttacking =
           Helpers.filter(
             ~f=x => self.state.time - x.startTime === 400,
             state.skeletons,
           );

         Array.mapi(
           (i, skeleton) => self.send(FlagSkeletonAsAttacking(skeleton)),
           skeletonsCurrentlyAttacking,
         );

         /* Every 100ms */
         if (state.time mod 100 === 0) {
           let occupiedLanes = ArrayLabels.map(s => s.lane, state.skeletons);

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
       },
     );

   let processInput = input =>
     ReasonReact.UpdateWithSideEffects(
       {...state, input},
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

             self.send(ReplaceWord(targetWord, Dictionary.getRandomWord()));
             self.send(KillSkeleton(self.state.time, targetLane));
             self.send(ClearInput);
           };
         },
     );

   let clearInput = () => ReasonReact.Update({...state, input: ""});
   let addWord = (randomWord, wordLane) => {
     let word: word = {randomWord, wordLane};
     let existingWords = state.words;
     switch (wordLane) {
     | Top => existingWords[0] = word
     | Middle => existingWords[1] = word
     | Bottom => existingWords[2] = word
     };
     ReasonReact.Update({...state, words: existingWords});
   };
   let replaceWord = (oldWord, newWord) => {
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
   };
   let spawnSkeleton = () => {
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
   };
   let killSkeleton = () => {
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
         self => {
           setTimeout(() => self.send(FlagSkeletonAsDead(lane)), 1000);
           ();
         },
       );
     };

     switch (Helpers.find(~f=x => x.lane === lane, state.skeletons)) {
     | skeleton => killIt(skeleton)
     | exception Not_found => ReasonReact.NoUpdate
     };
   };
   let flagSkeletonAsAttacking = () => {
     skeleton.status = Attacking;
     skeleton.stopTime = state.time - skeleton.startTime;

     ReasonReact.UpdateWithSideEffects(
       {...state, skeletons: state.skeletons},
       self => {
         let intervalId =
           Some(setInterval(() => self.send(DamagePlayer), 1000));
         self.send(SetSkeletonInterval(skeleton, intervalId));
       },
     );
   };
   let setSkeletonInterval = (skeleton, intervalId) => {
     skeleton.intervalId := intervalId;
     ReasonReact.Update({...state, skeletons: state.skeletons});
   };
   let flagSkeletonAsDead = () => {
     let flagIt = skeleton => {
       skeleton.status = Dead;
       switch (skeleton.intervalId^) {
       | Some(float) => clearInterval(float)
       | None => ()
       };

       ReasonReact.UpdateWithSideEffects(
         {...state, skeletons: state.skeletons},
         self => {
           setTimeout(() => self.send(RemoveSkeleton(lane)), 1000);
           ();
         },
       );
     };

     switch (Helpers.find(~f=x => x.lane === lane, state.skeletons)) {
     | skeleton => flagIt(skeleton)
     | exception Not_found => ReasonReact.NoUpdate
     };
   };
   let removeSkeleton = () => {
     let remainingSkeletons =
       Helpers.filter(~f=x => x.lane !== lane, state.skeletons);
     ReasonReact.Update({...state, skeletons: remainingSkeletons});
   };
   let damagePlayer = () =>
     ReasonReact.Update({...state, lives: state.lives - 1});
   let startGame = () =>
     ReasonReact.SideEffects(
       self => {
         let intervalId = Some(setInterval(() => Engine.tick(), 25));
         self.ReasonReact.state.intervalId := intervalId;
       },
     ); */