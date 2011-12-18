
class Scan {
    public static int scan() {
         try {
             return Integer.parseInt((new java.io.BufferedReader(
                             new java.io.InputStreamReader(System.in), 1)).readLine());
         } catch(Exception e) {
             return 0;
         }
    }
}
