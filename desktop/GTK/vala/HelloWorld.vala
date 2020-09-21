int main(string[] args)
{
    stdout.printf("Hello world\n");
    foreach (var arg in args) {
        stdout.printf(arg + "\n");
    }
    return 0;
}
