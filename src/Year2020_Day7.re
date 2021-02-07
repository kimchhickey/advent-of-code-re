open ReludeParse.Parser;
module P = {
  let let_ = bind;
};

let input = Node_fs.readFileAsUtf8Sync("input/Year2020_Day7.sample.txt");

let listToString = l => l->Belt.List.reduce("", (a, b) => a ++ b);

let wordParser = anyAlpha |> many <#> listToString;

let bagStringParser =
  str("bags.") <|> str("bags") <|> str("bag.") <|> str("bag");

let bagParser: t(string) = {
  let%P adjective = wordParser <* wsStr;
  let%P color = wordParser <* wsStr;
  let%P _ = bagStringParser;
  pure(adjective ++ " " ++ color);
};

let bagsParser: t((int, string)) =
  (anyUnsignedInt <* wsStr, bagParser)
  |> mapTuple2((num, bag) => (num, bag));

let bagsOrNoBagsParser =
  str("no other bags.") <#> (_ => []) <|> sepBy(str(", "), bagsParser);

type bag = (string, array((int, string)));

let sentenceParser: t(bag) =
  (bagParser, wsStr *> str("contain") <* wsStr, bagsOrNoBagsParser)
  |> mapTuple3((bag, _, bags) => (bag, bags->Belt.List.toArray));

let inputParser: t(array(bag)) = {
  sentenceParser <* eol |> many <#> Belt.List.toArray;
};

let bags = input->runParser(inputParser);

let rec isConnected = (adjacencyList: array(bag), source, target, toVisit) => {
  let (_, children) =
    adjacencyList
    ->Belt.Array.keep(((vertex, _)) => vertex == source)
    ->Belt.Array.getExn(0);

  if (children->Belt.Array.some(((_, vertex)) => vertex == target)) {
    true;
  } else {
    let toVisit =
      children->Belt.Array.reduce(toVisit, (toVisit, vertex) =>
        toVisit->Belt.List.add(vertex)
      );

    switch (toVisit) {
    | [] => false
    | [(_, bag), ...xs] => isConnected(adjacencyList, bag, target, xs)
    };
  };
};

let rec totalWeight = (adjacencyList, source) => {
  let (_, children) =
    adjacencyList
    ->Belt.Array.keep(((vertex, _)) => vertex == source)
    ->Belt.Array.getExn(0);

  if (children->Garter.Array.isEmpty) {
    0;
  } else {
    children->Belt.Array.reduce(0, (acc, (weight, vertex)) => {
      acc + weight + weight * totalWeight(adjacencyList, vertex)
    });
  };
};

// p1
Util.clog(
  bags->Belt.Result.flatMap(l => {
    let result =
      l
      ->Belt.Array.map(((vertex, _)) =>
          isConnected(l, vertex, "shiny gold", [])
        )
      ->Belt.Array.keep(v => v)
      ->Belt.Array.length;
    Belt.Result.Ok(result);
  }),
);

// p2
Util.clog(
  Belt.Result.flatMap(bags, l =>
    Belt.Result.Ok(totalWeight(l, "shiny gold"))
  ),
);
