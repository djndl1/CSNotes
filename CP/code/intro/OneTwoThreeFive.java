import java.util.ArrayList;
import java.util.Scanner;

class OneTwoThreeFive {
    public static void main(String[] args) {
        int n;
        var in = new Scanner(System.in);

        n = in.nextInt();
        var persons = new ArrayList<Person>();
        
        while (n-- > 0) {
            Person tmp = new Person();
            tmp.yyyy = in.nextInt();
            tmp.mm = in.nextInt();
            tmp.dd = in.nextInt();
            persons.add(tmp);
        }

        persons.sort((Person l, Person r) -> {
                if (l.mm != r.mm)
                    return l.mm - r.mm;
                else if (l.dd != r.dd)
                    return l.dd - r.dd;
                else
                    return r.yyyy - l.yyyy;
            });

        for (var p : persons) {
            System.out.println(p.mm + " " + p.dd + " " + p.yyyy);
        }
    }
}

class Person {
    public int dd;
    public int mm;
    public int yyyy;
}
