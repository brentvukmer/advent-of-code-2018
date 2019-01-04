package aoc.day9;

import clojure.lang.Keyword;
import org.magicwerk.brownies.collections.primitive.IntBigList;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class CircleGameSolver {

  private static boolean shouldTakeMarbles(int m) {
    return (m > 0) && (m % 23) == 0;
  }

  public static Map<Keyword, Object> solveFor(int numPlayers, int maxMarbleValue) {
    IntBigList circle = new IntBigList();
    circle.add(0);
    Map<Integer, IntBigList> playerScores = new HashMap<>(numPlayers);
    int currentIndex = 0;
    int bound = maxMarbleValue + 1;
    for (int m = 1; m < bound; m++) {
      if (shouldTakeMarbles(m)) {
        int player = m % numPlayers;
        int removeIndex = Math.abs((currentIndex - 7) % circle.size());
        int removedMarble = circle.get(removeIndex);
        circle.remove(removeIndex);
        playerScores.putIfAbsent(player, new IntBigList());
        IntBigList scores = playerScores.get(player);
        scores.add(m);
        scores.add(removedMarble);
        currentIndex = removeIndex;
      } else {
        int nextIndex = ((currentIndex + 1) % circle.size()) + 1;
        circle.add(nextIndex, m);
        currentIndex = nextIndex;
      }
    }
    Map<Keyword, Object> result = new HashMap<>();
    result.put(Keyword.intern(null, "circle"), circle);
    List<BigInteger> gameScores =
        playerScores
            .values()
            .stream()
            .map(
                l -> {
                  BigInteger sum = BigInteger.ZERO;
                  for (int i = 0; i < l.size(); i++) {
                    sum = sum.add(BigInteger.valueOf(l.get(i)));
                  }
                  return sum;
                })
            .sorted()
            .collect(Collectors.toList());
    result.put(Keyword.intern(null, "game-scores"), gameScores);
    result.put(Keyword.intern(null, "high-score"), gameScores.get(gameScores.size() - 1));
    result.put(Keyword.intern(null, "player-marbles"), playerScores);
    return result;
  }
}
