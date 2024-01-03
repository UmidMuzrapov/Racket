public class tryType {
   public static void main(String args[]) {
      printClass('c' / 'c');
    }
   private static void printClass(Object o) {
      System.out.println(o.getClass().getName());
   }
}
