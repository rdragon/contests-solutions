// This solution was written on 2021-05-13 during a practice round.

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;

namespace TestingMono
{
    static class HackedExam
    {
        static Dictionary<int, BigInteger> _combinations;

        static void Main()
        {
            ComputeCombinations();
            var testCount = int.Parse(ReadLine());

            for (int testNumber = 1; testNumber <= testCount; testNumber++)
            {
                RunTest(testNumber);
            }
        }

        static void ComputeCombinations()
        {
            var combinations = new Dictionary<int, BigInteger>();
            var factorials = new List<BigInteger>() { 1, 1 };

            for (int i = 2; i <= 120; i++)
            {
                factorials.Add(factorials[i - 1] * i);
            }

            for (int n = 0; n <= 120; n++)
            {
                for (int k = 0; k <= n; k++)
                {
                    var key = (n << 10) | k;
                    combinations[key] = factorials[n] / (factorials[k] * factorials[n - k]);
                }
            }

            _combinations = combinations;
        }

        static void RunTest(int testNumber)
        {
            var studentCount = int.Parse(ReadLine().Split(' ')[0]);
            var exams = new List<Exam>();

            for (int i = 0; i < studentCount; i++)
            {
                var parts = ReadLine().Split(' ');
                var answers = parts[0].Select(c => c == 'T').ToArray();
                var score = int.Parse(parts[1]);
                var exam = new Exam { Answers = answers, Score = score };
                exams.Add(exam);
            }

            while (exams.Count < 3)
            {
                exams.Add(exams[0].Clone());
            }

            var shouldFlips = exams[0].Answers.ToArray();

            foreach (var exam in exams)
            {
                exam.FlipAnswers(shouldFlips);
            }

            var result = GetResult(exams.ToArray());
            result.Exam.FlipAnswers(shouldFlips);

            var answerString = new string(result.Exam.Answers.Select(b => b ? 'T' : 'F').ToArray());

            Console.WriteLine($"Case #{testNumber}: {answerString} {result.Numerator}/{result.Denominator}");
        }

        static Result GetResult(Exam[] exams)
        {
            if (exams[0].Answers.Any(b => b))
            {
                throw new Exception($"Expecting the first exam to answer False to all questions.");
            }

            var groups = new Dictionary<int, List<int>>(); // Maps the group keys (0, 1, 2, 4) to the list of questions (indices) that are contained in the group.
            var questionCount = exams[0].Answers.Length;
            var score1 = exams[0].Score;
            var finalFalseCounts = new BigInteger[questionCount];

            for (int i = 0; i < 4; i++)
            {
                groups[i] = new List<int>();
            }

            for (int i = 0; i < questionCount; i++)
            {
                var keyPart1 = exams[0].Answers[i] == exams[1].Answers[i] ? 0 : 1;
                var keyPart2 = exams[0].Answers[i] == exams[2].Answers[i] ? 0 : 2;
                var key = keyPart1 + keyPart2;
                groups[key].Add(i);
            }

            var questionToGroup = new int[questionCount];

            foreach (var pair in groups)
            {
                foreach (var question in pair.Value)
                {
                    questionToGroup[question] = pair.Key;
                }
            }

            var count1 = groups[0].Count; // The number of questions inside group 1.
            var count2 = groups[1].Count;
            var count3 = groups[2].Count;
            var count4 = groups[3].Count;
            var max1 = Math.Min(count1, score1); // The maximum value for good1.
            var totalScoreB = exams[1].Score;
            var totalScoreC = exams[2].Score;
            BigInteger totalWays = 0;

            // The value good1 represents the number of good answers in the first exam inside the first group.
            // This value is also equal to the number of False answers in the first group (as the first exam answered False to all questions).
            for (int good1 = 0; good1 <= max1; good1++)
            {
                if (good1 > totalScoreB || good1 > totalScoreC)
                {
                    continue;
                }

                var score2 = score1 - good1; // The score that is left and that should be fulfilled by good2, good3, good4.
                var max2 = Math.Min(count2, score2); // The maximum value for good2.
                var ways1 = GetCombinationCount(count1, good1); // The number of ways it is possible to get exactly good1 good in the first group.

                // The value good2 represents the number of good answers in the first exam inside the second group.
                for (int good2 = 0; good2 <= max2; good2++)
                {
                    var scoreB_2 = good1 + count2 - good2;
                    var scoreC_2 = good1 + good2;

                    if (scoreB_2 > totalScoreB || scoreC_2 > totalScoreC)
                    {
                        continue;
                    }

                    var score3 = score2 - good2;
                    var max3 = Math.Min(count3, score3);
                    var ways2 = GetCombinationCount(count2, good2) * ways1; // The number of ways it is possible to get exactly good1 good in the first group and good2 good in the second group.

                    for (int good3 = 0; good3 <= max3; good3++)
                    {
                        var scoreB_3 = scoreB_2 + good3;
                        var scoreC_3 = scoreC_2 + count3 - good3;

                        if (scoreB_3 > totalScoreB || scoreC_3 > totalScoreC)
                        {
                            continue;
                        }

                        var ways3 = GetCombinationCount(count3, good3) * ways2;
                        var score4 = score3 - good3;

                        if (score4 > count4)
                        {
                            continue;
                        }

                        var good4 = score4;
                        var scoreB_4 = scoreB_3 + count4 - good4;
                        var scoreC_4 = scoreC_3 + count4 - good4;

                        if (scoreB_4 != totalScoreB || scoreC_4 != totalScoreC)
                        {
                            continue;
                        }

                        var ways4 = GetCombinationCount(count4, good4) * ways3;
                        totalWays += ways4;

                        var falseCounts = new BigInteger[]
                        {
                                ways4 * good1 / Math.Max(count1, 1),
                                ways4 * good2 / Math.Max(count2, 1),
                                ways4 * good3 / Math.Max(count3, 1),
                                ways4 * good4 / Math.Max(count4, 1),
                        };

                        for (int question = 0; question < questionCount; question++)
                        {
                            var group = questionToGroup[question];
                            var falseCount = falseCounts[group];
                            finalFalseCounts[question] += falseCount;
                        }
                    }
                }
            }

            var resultAnswers = new bool[questionCount];
            BigInteger rawNumerator = 0;

            for (int question = 0; question < questionCount; question++)
            {
                var falseCount = finalFalseCounts[question];
                var trueCount = totalWays - falseCount;
                rawNumerator += BigInteger.Max(falseCount, trueCount);
                resultAnswers[question] = trueCount > falseCount;
            }

            var divisor = BigInteger.GreatestCommonDivisor(rawNumerator, totalWays);

            return new Result
            {
                Exam = new Exam { Answers = resultAnswers },
                Denominator = totalWays / divisor,
                Numerator = rawNumerator / divisor,
            };
        }

        static BigInteger GetCombinationCount(int n, int k)
        {
            var key = (n << 10) | k;

            return _combinations[key];
        }

        class Exam
        {
            public bool[] Answers { get; set; }
            public int Score { get; set; }

            public Exam Clone()
            {
                return new Exam { Answers = Answers.ToArray(), Score = Score };
            }

            public void FlipAnswers(bool[] shouldFlips)
            {
                for (int i = 0; i < shouldFlips.Length; i++)
                {
                    if (shouldFlips[i])
                    {
                        Answers[i] = !Answers[i];
                    }
                }
            }
        }

        class Result
        {
            public Exam Exam { get; set; }
            public BigInteger Numerator { get; set; }
            public BigInteger Denominator { get; set; }
        }

#if DEBUG
        static Queue<string> _lines;

        static string ReadLine()
        {
            if (_lines == null)
            {
                _lines = new Queue<string>(File.ReadAllLines("C:/kw/in.txt"));
            }

            return _lines.Dequeue();
        }
#else
        static string ReadLine()
        {
            return Console.ReadLine();
        }
#endif
    }
}
