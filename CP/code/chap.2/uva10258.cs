using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class UVA10258
{
    public static void Main()
    {
        using (var sin = new StreamReader(Console.OpenStandardInput()))
            using (var sout = new StreamWriter(Console.OpenStandardOutput()))
            {
                string temp = sin.ReadLine();
                int cases = Convert.ToInt32(temp);
                sin.ReadLine();

                for (int cas = 0; cas < cases; cas++)
                {
                    var teams = new List<Contestant>(100);
                    while (true)
                    {
                        string curLine = sin.ReadLine();
                        if (String.IsNullOrWhiteSpace(curLine))
                            break;

                        var items = curLine.Trim().Split(new char[] {' '}, StringSplitOptions.RemoveEmptyEntries);
                        var sub = new Submission()
                            {
                                contestantNumber = Convert.ToInt32(items[0]),
                                problemNumber = Convert.ToInt32(items[1]),
                                time = Convert.ToInt32(items[2]),
                                result = items[3],
                            };

                        int teamPos = teams.FindIndex((team) => team.ContestantNumber == sub.contestantNumber);
                        if (teamPos == -1)
                        {
                            var newTeam = new Contestant(sub.contestantNumber);
                            newTeam.AddProblem(sub.problemNumber, sub.time, sub.result);
                            teams.Add(newTeam);
                        }
                        else
                        {
                            teams[teamPos].AddProblem(sub.problemNumber, sub.time, sub.result);
                        }
                    }
                    teams.Sort(new ContestantRanker());

                    if (cas != 0)
                        sout.Write(Environment.NewLine);
                    foreach (var team in teams)
                    {
                        sout.Write(team + Environment.NewLine);
                    }
                }
            }
    }
}

class Submission
{
    public int contestantNumber;
    public int problemNumber;
    public int time;
    public string result;
}

class SolvingStatus
{
    public bool corrected;
    public int penalty;
}

class Contestant
{
    private List<SolvingStatus> problemStatuses;

    public int ContestantNumber => contestantNumber;
    private int contestantNumber;

    public int ProblemsSolved => problemStatuses.Count((status) => status.corrected);

    public int PenaltyTime => problemStatuses.Sum((status) => status.corrected ? status.penalty : 0);

    public Contestant(int teamNumber)
    {
        problemStatuses = new List<SolvingStatus>(10);
        for (int i = 0; i < 10; i++)
            problemStatuses.Add(new SolvingStatus());
        contestantNumber = teamNumber;
    }

    public void AddProblem(int problemNumber, int time, string res)
    {
        if (problemNumber >= problemStatuses.Count ||
            problemNumber < 0)
            throw new IndexOutOfRangeException("Problem Number Not Expected");
        if (!problemStatuses[problemNumber].corrected)
        {
            if (res == "C")
                problemStatuses[problemNumber].penalty += time;
            else if (res == "I")
                problemStatuses[problemNumber].penalty += 20;
            problemStatuses[problemNumber].corrected = (res == "C") ? true : false;
        }
    }

    public override string ToString()
    {
        return contestantNumber + " " + ProblemsSolved + " " + PenaltyTime;
    }
}

class ContestantRanker: Comparer<Contestant>
{
    public override int Compare(Contestant x, Contestant y)
    {
        if (x.ProblemsSolved > y.ProblemsSolved)
            return -1;
        else if (x.ProblemsSolved < y.ProblemsSolved)
            return 1;
        else
        {
            if (x.PenaltyTime < y.PenaltyTime)
                return -1;
            else if (x.PenaltyTime > y.PenaltyTime)
                return 1;
            else
                return x.ContestantNumber.CompareTo(y.ContestantNumber);
        }
    }
}
