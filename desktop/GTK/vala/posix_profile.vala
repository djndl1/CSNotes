[Compact]
class CompactClass {
    public int a;

    public CompactClass(int arg)
    {
        a = arg;
    }
}

int main(string[] args)
{
    var cc = new CompactClass(5);
    stdout.printf("%d\n", cc.a);

    return 0;
}
