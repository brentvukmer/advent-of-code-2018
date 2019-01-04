package aoc.day9;

import clojure.lang.Keyword;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class CircleGameSolver {

    private static boolean shouldTakeMarbles(int m) {
        return (m > 0) && (m % 23) == 0;
    }

    public static Map<Keyword, Object> solveFor(int numPlayers, int maxMarbleValue) {
        List<Integer> circle = new ArrayList<>(maxMarbleValue + 1);
        circle.add(0);
        Map<Integer, Integer> playerScore = new HashMap<>(numPlayers);
        int currentIndex = 0;
        int bound = maxMarbleValue + 1;
        for (int m = 1; m < bound; m++) {
            if (shouldTakeMarbles(m)) {
                int player = m % numPlayers;
                int removeIndex = Math.abs((currentIndex - 7) % circle.size());
                int removedMarble = circle.get(removeIndex);
                circle.remove(removeIndex);
                playerScore.putIfAbsent(player, 0);
                Integer score = playerScore.get(player);
                playerScore.put(player, score + m + removedMarble);
                currentIndex = removeIndex;
            } else {
                int nextIndex = ((currentIndex + 1) % circle.size()) + 1;
                circle.add(nextIndex, m);
                currentIndex = nextIndex;
            }
        }
        Map<Keyword, Object> result = new HashMap<>();
        result.put(Keyword.intern(null, "circle"), circle);
        List<Integer> gameScores = playerScore
                .values()
                .stream()
                .sorted()
                .collect(Collectors.toList());
        result.put(Keyword.intern(null, "high-score"), gameScores.get(gameScores.size() - 1));
        result.put(Keyword.intern(null, "player-marbles"), playerScore);
        return result;
    }

}
