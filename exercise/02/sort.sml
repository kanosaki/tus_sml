fun curry f x y = f (x, y);

fun take_while p nil = nil
  | take_while p (x::xs) = if p x then x::take_while p xs else nil;
fun drop_while p nil = nil
  | drop_while p (x::xs) = if p x then drop_while p xs else x::xs;

fun insert (x, xs) = 
  (take_while (curry op>= x) xs) @ [x] @ (drop_while (curry op>= x) xs);
fun isort nil = nil
  | isort xs = foldr insert nil xs;

val seed = Random.rand(0, 10000000)
fun generate s 0 = nil
  | generate s n = Random.randInt s :: generate s (n-1)

fun bench f = 
let val start = Time.now()
in
  (f(); Time.- (Time.now(), start))
end

fun range st en delta = 
  if st <= en
    then st :: (range (st + delta) en delta)
    else nil
val default_range = range 0 1000 10
fun exec_sort src (_:unit) = isort src
fun source_data ran = map (generate seed) ran
fun execute_bench r = ListPair.zip (r, map (bench o exec_sort) (source_data r))

fun output fp nil = (TextIO.flushOut fp)
  | output fp ((n, time)::ds) = 
  let
    val t = Int.toString((Int.fromLarge o Time.toMicroseconds)(time))
  in
    (TextIO.output(fp, Int.toString(n)^","^t^"\n");
    output fp ds)
  end;

fun openfile s = TextIO.openOut s

fun exec (_:unit) = let
  val fp = TextIO.openOut "sortlog"
  val ran = default_range
in
  (output fp (execute_bench ran); TextIO.closeOut fp)
end;
