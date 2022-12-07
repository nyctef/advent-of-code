using System;
using System.IO;
using System.Linq;
using BenchmarkDotNet;
using BenchmarkDotNet.Attributes;

namespace temp
{
    [ShortRunJob]
    public class Benchmarks
    {
        public static string InputText => File.ReadAllText("6-1.txt");

        [Benchmark]
        public void Distinct_and_sequenceequal()
        {
            for (var i = 14; i < InputText.Length; i++)
            {
                var seq = InputText[(i - 14)..i];
                if (seq.Distinct().SequenceEqual(seq))
                {
                    //Console.WriteLine(i);
                    return;
                }
            }
        }

        [Benchmark]
        public void Span_and_bitmask_distinct()
        {
            int mask = 0;
            bool unique;
            var span = InputText.AsSpan();
            for (var i = 14; i < InputText.Length - 1; i++)
            {
                var seq = span[(i - 14)..i];
                unique = true;
                mask = 0;
                for (var j = 0; j < seq.Length; j++)
                {
                    var bit = 1 << seq[j];
                    if ((mask & bit) != 0)
                    {
                        unique = false;
                        break;
                    }
                    mask |= bit;
                }

                if (unique)
                {
                    // Console.WriteLine(i);
                    return;
                }

            }
            // Implement your benchmark here
        }


        [Benchmark]
        public void Skip_ahead_for_seen_letters()
        {
            var lastSeenPositionForLetter = new int[26];
            var target = 14;
            var countSinceLastDuplicateLetter = 0;
            for (var i = 0; i < InputText.Length - 10;)
            {
                Console.Write(i + " " + InputText[i] + " " + (InputText[i] - 'a') + " ");
                int off = InputText[i] - 'a';
                if (i < target)
                {
                    lastSeenPositionForLetter[off] = i;
                    i++;
                    continue;
                }
                else
                {
                    if (i - lastSeenPositionForLetter[off] < target)
                    {
                        i = lastSeenPositionForLetter[off];
                        countSinceLastDuplicateLetter = 0;
                    }
                    else
                    {
                        countSinceLastDuplicateLetter++;
                    }

                    Console.WriteLine(countSinceLastDuplicateLetter + " ");

                    if (countSinceLastDuplicateLetter == target)
                    {
                        Console.WriteLine(i);
                        break;
                    }

                    lastSeenPositionForLetter[off] = i;
                    i++;
                }
            }

            Console.WriteLine("failed");
        }

    }
}
