package aoc.day9;

import clojure.lang.Keyword;
import org.magicwerk.brownies.collections.primitive.IntBigList;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.math.BigInteger.ZERO;
import static java.math.BigInteger.valueOf;

public class CircleGameSolver {

  private static boolean shouldTakeMarbles(int m) {
    return (m > 0) && (m % 23) == 0;
  }

  public static Map<Keyword, Object> solveFor(int numPlayers, int maxMarbleValue) {
    IntBigList circle = new IntBigList();
    circle.add(0);
    Map<Integer, BigInteger> playerScore = new HashMap<>(numPlayers);
    int currentIndex = 0;
    int bound = maxMarbleValue + 1;
    for (int m = 1; m < bound; m++) {
      if (shouldTakeMarbles(m)) {
        int player = m % numPlayers;
        int removeIndex = Math.abs((currentIndex - 7) % circle.size());
        int removedMarble = circle.get(removeIndex);
        circle.remove(removeIndex);
        playerScore.putIfAbsent(player, ZERO);
        BigInteger updatedScore =
            playerScore.get(player).add(valueOf(m)).add(valueOf(removedMarble));
        playerScore.put(player, updatedScore);
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
        playerScore.values().stream().sorted().collect(Collectors.toList());
    result.put(Keyword.intern(null, "high-score"), gameScores.get(gameScores.size() - 1));
    result.put(Keyword.intern(null, "player-marbles"), playerScore);
    return result;
  }
}
