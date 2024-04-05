scratch= save(scratch, tmp);
tmp= save(data, push, pop, unshift, shift, first, last,
           count, help);

func deque (base, data, ..)
  /* DOCUMENT dq= deque();
              dq= deque(grp_obj); OR
              dq= deque(grp_obj1, grp_obj2); // left to righ append to data

     Double-ended queue ("deque") object DQ.
     Queue elements are stored in the 'dq(data)' member.

     In the 1st form, 'dq= deque()', DQ is instanciated as an empty deque.
     In the 2nd/3rd form(s), 'dq= deque(grp_obj{i})', GRP_OBJ{i} are group object
     listing initial members to store in the deque.

     DATA: > g= dq(data);
     G is the DQ data member, an oxy group. Deque items are stored in that object.
     Items are added/removed by deque methods and they are stored anonymously,
     and as shallow copy.

     In the description below:
     START member is first(1), and END is last(0) member, DATA(1) and DATA(0)
     of the group/list.

     PUSH/POP adds/removes at the END of the list.
     UNSHIFT/SHIFT add/removes at START of the list.

     The deque object is comprised of a single data member DATA and six
     methods: PUSH, POP, UNSHIFT, SHIFT, FIRST, LAST, COUNT

     PUSH: > dq, push, item1, item2, item3, ...;
           > ndq= dq(push, item1, item2, item3, ...);
     Pushes one or more items onto the **END** of the DQ -- appending to DQ(DATA).
     As a function, PUSH returns the total number of items in the deque NDQ.
     MULTIPLE arguments: PUSH is repeated in order LEFT-to-RIGHT

     POP: > dq, pop;
          > it= dq(pop,);
     Removes at the **END** (item IT) of the deque.  Returns IT *if* POP is called
     as a function, otherwise discarded. If no items are in the deque, this is a
     no-op and [] will be returned.

     UNSHIFT: > dq, unshift, item1, item2, item3, ...;
              > ndq= dq(unshift, item1, item2, item3, ...);
     Add one or more items onto the **START** of the deque.
     As a function, UNSHIFT returns the total number of items in the deque NDQ.
     MULTIPLE arguments: UNSHIFT is repeated in order LEFT-to-RIGHT

     SHIFT: > dq, shift;
            > it= dq(shift,);
     Removes the first item IT from the deque. Returns IT if SHIFT is called as
     a function, otherwise discarded. If no items are in the deque, this is a
     no-op and [] will be returned.

     FIRST: LAST:
            > it1= dq(first,);
            > it0= dq(last,);
     Returns the first IT1 or last IT0 item of the deque, or [] if DQ is empty.

     COUNT: > ndq= dq(count,);
     Returns the number of items in the deque NDQ.

     UNSHIFT and PUSH take a EL=1 keyword to signify repeat operation (unshift/push)
     for every group elements, IFF a group is in the argument list.

     A deque can serve as a stack by only using push/pop. It can serve as a queue
     by only using unshift/shift.

  */
{
  obj= base(:);
  data= is_void(data)? save(): data;
  while(more_args())
    save, data, [], next_arg();
  save, obj, data;
  return obj;
}

func push (val, .., el=) {
  use, data;
  if (el==1)
    save, data, [], val;
  else
    save, data, string(0), val;
  while(more_args())
    if (el==1)
      save, data, [], next_arg();
    else
      save, data, string(0), next_arg();
  return data(*);
}

func pop (nil) {
  use, data;
  result= data(*)? data(0): [];
  data= (data(*)>1? data(:-1): save());
  return result;
}

func unshift (val, .., el=) {
  use, data;
  count= data(*);
  oadd= el==1? save([],val): save(string(0),val)
  while(more_args())
    if (el==1)
      save, oadd, [], next_arg();
    else
      save, oadd, string(0), next_arg();
  oadd= oadd(::-1)
  save,oadd,[],data;
  data= oadd;
  return data(*);
}

func shift (nil) {
  use, data;
  result= data(*)? data(1): [];
  data= (data(*)>1? data(2:): save());
  return result;
}

func first (nil) {
  use, data;
  return data(*)? data(1): [];
}

func last (nil) {
  use, data;
  return data(*)? data(0): [];
}

func count (nil) {
  use, data;
  return data(*);
}
help= closure(help, deque);
deque= closure(deque, restore(tmp));
restore, scratch;

#if 1
d= deque();
d,push,1,2,3,4;
d,push,5;
d,push,6,7;
i= 0;
while (d(pop,)==(7-i++)){};
if (d(data,*)!=0)
  error,"deque push/pop";

for (i=1,o=save(); i<=4; i++)
  save,o,string(0),i;
d,push,o,el=1;
d,push,5;
d,push,6,7;
i= 0;
while (d(pop,)==(7-i++)){};
if (d(data,*)!=0)
  error,"deque push/pop el=1";

for (i=1,o=save(); i<=4; i++)
  save,o,string(0),i;
d= deque(o);
d,push,5;
d,push,6,7;
i= 0;
while (d(pop,)==(7-i++)){};
if (d(data,*)!=0)
  error,"deque push/pop dq(grp) init";

d,unshift,7,6
d,unshift,5;
d,unshift,4,3,2,1;
i= 1;
while (d(shift,)==i++){};
if (d(data,*)!=0)
  error,"deque push/pop dq(grp) init";

     // 5 4 3 2 1
     // > d,unshift,5; d,unshift,1,2,3,4; while(write(d(shift,),format="%i ")){}; write,"";
     // 1 2 3 4 5

#endif
