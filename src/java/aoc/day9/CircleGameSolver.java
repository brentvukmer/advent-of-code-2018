package aoc.day9;

import clojure.lang.Keyword;
import clojure.lang.RT;
import org.magicwerk.brownies.collections.primitive.LongBigList;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.math.BigInteger.*;

public class CircleGameSolver {

    public static boolean shouldTakeMarbles(long m) {
        return (m > 0) && (m % 23) == 0;
    }

    public static Long gaussianMod(Long num, Long div) {
        return (Long) RT.var("clojure.core", "mod").invoke(num, div);
    }

    public static Map<Keyword, Object> solveFor(int numPlayers, int maxMarbleValue) {
        LongBigList circle = new LongBigList();
        circle.add(0);
        Map<Long, BigInteger> playerScores = new HashMap<>(numPlayers);
        long currentIndex = 0;
        long bound = maxMarbleValue + 1;
        for (long m = 1; m < bound; m++) {
            if (shouldTakeMarbles(m)) {
                long player = gaussianMod(m, (long) numPlayers);
                long removeIndex = gaussianMod((currentIndex - 7), (long) circle.size());
                long removedMarble = circle.get((int) removeIndex);
                circle.remove((int) removeIndex);
                playerScores.putIfAbsent(player, ZERO);
                BigInteger updatedScore = playerScores.get(player)
                        .add(BigInteger.valueOf(m))
                        .add(BigInteger.valueOf(removedMarble));
                playerScores.put(player, updatedScore);
                currentIndex = removeIndex;
            } else {
                long nextIndex = getNextClockwiseIndex(circle, currentIndex) + 1;
                circle.add((int) nextIndex, m);
                currentIndex = nextIndex;
            }
        }
        Map<Keyword, Object> result = new HashMap<>();
        result.put(Keyword.intern(null, "circle"), circle);
        List<BigInteger> gameScores =
                playerScores
                        .values()
                        .stream()
                        .sorted()
                        .collect(Collectors.toList());
        result.put(Keyword.intern(null, "game-scores"), gameScores);
        result.put(Keyword.intern(null, "high-score"), gameScores.get(gameScores.size() - 1));
        result.put(Keyword.intern(null, "player-marbles"), playerScores);
        return result;
    }

    private static long getNextClockwiseIndex(LongBigList circle, long currentIndex) {
        return gaussianMod((currentIndex + 1), (long) circle.size());
    }
}
