scratch= save(scratch, tmp);
tmp= save(data, push, pop, unshift, shift, first, last,
           count, help);

func deque (base, data)
  /* DOCUMENT dq= deque();
              dq= deque(grp_obj);
     Creates a double-ended queue ("deque") object DQ.
     In the form 'dq= deque()' DQ is an empty deque.
     In the second form dq= deque(grp_obj); GRP_OBJ must be a group object that
     serves as the initial data to store within the deque (as its "data" member).

     The stack is an group object DATA. TOP/BEGIN is first, BOTTOM/END is last
     member, DATA(1) and DATA(0), resp.
     The deque object is comprised of a single data member DATA and seven
     methods: PUSH, POP, UNSHIFT, SHIFT, FIRST, LAST, COUNT, HELP

     HELP:
     dq, help;
     Display documentation.

     DATA:
     g= dq(data);
     G is the DQ data member, an oxy group. Deque items are stored in that object.
     Items are added/removed by deque methods and are stored  anonymously.

     PUSH:
     dq, push, item1, item2, item3, ...;
     ndq= dq(push, item1, item2, item3, ...);
     Pushes one or more items onto the **end** of the DQ -- appending to DQ(DATA).
     As a function, PUSH returns the total number of items in the deque NDQ.
     Pushing order: is **Left-to-right**, last/rightmost is last/end on queue.

     POP:
     dq, pop;
     it= dq(pop,);
     Removes the **last** item IT from the deque.  Returns IT *if* POP is called
     as a function, otherwise discarded. If no items are in the deque, this is a
     no-op and [] will be returned.

     UNSHIFT:
     dq, unshift, item1, item2, item3, ...;
     ndq= dq(unshift, item1, item2, item3, ...);
     Add one or more items onto the **front** of the deque. Their order is
     maintained, so that the first item given becomes the first item in the
     deque, the second item given becomes the second item in the deque, and
     so on. If called as a function, returns the total number of items in
     the deque.

     SHIFT:
     dq, shift;
     it= dq(shift,);
     Removes the first item IT from the deque. Returns IT if SHIFT is called as
     a function, otherwise discarded. If no items are in the deque, this is a
     no-op and [] will be returned.

     FIRST: LAST:
     it1= dq(first,);
     it0= dq(last,);
     Returns the first IT1 or last IT0 item of the deque, or [] if DQ is empty.

     ndq= dq(count,);
     Returns the number of items in the deque NDQ.

     A deque can serve as a stack by only using push/pop. It can serve as a queue
     by only using unshift/shift.

     examples:
     > d,push,1,2,3,4; d,push,5; while(write(d(pop,),format="%i ")){}; write,"";
     5 4 3 2 1
     > d,unshift,5; d,unshift,1,2,3,4; while(write(d(shift,),format="%i ")){}; write,"";
     1 2 3 4 5
  */
{
  obj= base(:);
  data= is_void(data)? save(): data;
  save, obj, data;
  return obj;
}
func push (val, ..) {
  use, data;
  save, data, string(0), val;
  while(more_args())
    save, data, string(0), next_arg();
  return data(*);
}
func pop (nil) {
  use, data;
  result= data(*) ? data(0) : [];
  data= (data(*) > 1 ? data(:-1) : save());
  return result;
}
func unshift (val, ..) {
  use, data;
  count= data(*);
  save, data, string(0), val;
  while(more_args())
    save, data, string(0), next_arg();
  nd= data(*);
  data= data(_(indgen(count+1:nd),indgen(count)));
  return data(*);
}
func shift (nil) {
  use, data;
  result= data(*) ? data(1) : [];
  data= (data(*) > 1 ? data(2:) : save());
  return result;
}
func first (nil) {
  use, data;
  return data(*) ? data(1) : [];
}
func last (nil) {
  use, data;
  return data(*) ? data(0) : [];
}
func count (nil) {
  use, data;
  return data(*);
}
help= closure(help, deque);
deque= closure(deque, restore(tmp));
restore, scratch;
