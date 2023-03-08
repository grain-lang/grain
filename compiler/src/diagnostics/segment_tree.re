/*
   This data type is a Segment Tree, a balanced binary tree which gives us
   O(log n) lookups of matching intervals. You can learn more about Segment
   Trees here:

   https://en.wikipedia.org/wiki/Segment_tree
 */

module type OrderableSegment = {
  include Set.OrderedType;

  type segment = (t, t);
  let center: (t, t) => t;
  let compare_segments: (segment, segment) => int;
};

module type SegmentTree = {
  type t('a);
  type point;
  type segment = (point, point);

  let create: list((segment, 'a)) => t('a);
  let query: (point, t('a)) => list('a);
};

module Make = (Ord: OrderableSegment) => {
  type point = Ord.t;
  type segment = Ord.segment;

  type t('a) =
    | Leaf
    | Node({
        center: point,
        data: list((segment, 'a)),
        left: t('a),
        right: t('a),
      });

  let center = segments => {
    let rec center = (left_acc, right_acc, segments) => {
      switch (segments) {
      | [((first, last), _), ...rest] =>
        let left =
          if (Ord.compare(first, left_acc) < 0) {
            first;
          } else {
            left_acc;
          };
        let right =
          if (Ord.compare(last, right_acc) > 0) {
            last;
          } else {
            right_acc;
          };
        center(left, right, rest);
      | [] => Ord.center(left_acc, right_acc)
      };
    };
    switch (segments) {
    | [((first, last), _), ...rest] => center(first, last, rest)
    | [] => failwith("no segments provided")
    };
  };

  let rec create = segments => {
    switch (segments) {
    | [] => Leaf
    | segments =>
      let pivot = center(segments);
      let rec split = (left_acc, center_acc, right_acc, segments) => {
        switch (segments) {
        | [] => (left_acc, center_acc, right_acc)
        | [((first, last), _) as k, ...rest]
            when Ord.compare(last, pivot) < 0 =>
          split([k, ...left_acc], center_acc, right_acc, rest)
        | [((first, last), _) as k, ...rest]
            when Ord.compare(first, pivot) > 0 =>
          split(left_acc, center_acc, [k, ...right_acc], rest)
        | [((first, last), _) as k, ...rest] =>
          split(left_acc, [k, ...center_acc], right_acc, rest)
        };
      };
      let (left, center, right) = split([], [], [], segments);
      Node({
        center: pivot,
        data: center,
        left: create(left),
        right: create(right),
      });
    };
  };

  let in_segment = ((low, high), candidate) => {
    Ord.compare(low, candidate) <= 0 && Ord.compare(candidate, high) < 0;
  };

  let query = (point, tree) => {
    let rec query = (acc, tree) => {
      switch (tree) {
      | Leaf => acc
      | Node({center, data, left, right}) =>
        let results =
          List.filter(
            ((segment, _)) => {in_segment(segment, point)},
            data,
          );
        if (Ord.compare(point, center) < 0) {
          query(results @ acc, left);
        } else {
          query(results @ acc, right);
        };
      };
    };
    query([], tree);
  };

  let query = (x, tree) => {
    query(x, tree)
    |> List.sort(((segment_x, _), (segment_y, _)) =>
         Ord.compare_segments(segment_x, segment_y)
       )
    |> List.map(((_, payload)) => payload);
  };
};