open ReludeParse.Parser;

module P = {
  let let_ = bind;
};

let input = Node_fs.readFileAsUtf8Sync("input/Year2020_Day7.txt");

type bag = (string, array((int, string)));

let bagParser: t(string) = {
  let wordParser =
    anyAlpha |> many <#> (l => l->Belt.List.reduce("", (a, b) => a ++ b));

  let%P adjective = wordParser <* wsStr;
  let%P color = wordParser <* wsStr;
  let%P _ = str("bags.") <|> str("bags") <|> str("bag.") <|> str("bag");
  pure(adjective ++ " " ++ color);
};

let containParser = wsStr *> str("contain") <* wsStr;

let bagsParser: t(array((int, string))) = {
  let innerBagParser =
    (anyUnsignedInt <* wsStr, bagParser)
    |> mapTuple2((num, bag) => (num, bag));

  str("no other bags.")
  <#> (_ => [||])
  <|> (sepBy(str(", "), innerBagParser) <#> (l => l->Belt.List.toArray));
};

let sentenceParser: t(bag) = {
  (bagParser, containParser, bagsParser)
  |> mapTuple3((bag, _, bags) => (bag, bags));
};

let inputParser: t(array(bag)) = {
  sentenceParser <* eol |> many <#> Belt.List.toArray;
};

let bags = input->runParser(inputParser);

let rec isConnected = (adjacencyList, source, target, toVisit) => {
  let (_, children) =
    adjacencyList
    ->Belt.Array.getBy(((vertex, _)) => vertex == source)
    ->Belt.Option.getExn;

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
    ->Belt.Array.getBy(((vertex, _)) => vertex == source)
    ->Belt.Option.getExn;

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
  bags->Belt.Result.flatMap(bags => {
    let result =
      bags
      ->Belt.Array.keep(((vertex, _)) =>
          isConnected(bags, vertex, "shiny gold", [])
        )
      ->Belt.Array.length;
    Belt.Result.Ok(result);
  }),
);

// p2
Util.clog(
  Belt.Result.flatMap(bags, bags =>
    Belt.Result.Ok(totalWeight(bags, "shiny gold"))
  ),
);
