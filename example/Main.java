public class Main {
  public String x (I a) { return a.m(); }

  public static void main (String [] args) {
    System.out.println(new Main().x(new A()));
  }
}
