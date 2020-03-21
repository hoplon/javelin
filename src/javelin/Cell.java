package javelin;

import java.util.List;
import java.util.HashSet;
import java.util.ArrayList;
import java.lang.ref.WeakReference;
import clojure.java.api.Clojure;
import clojure.lang.IFn;
import clojure.lang.IRef;
import clojure.lang.ISeq;
import clojure.lang.IAtom;
import clojure.lang.IPersistentMap;
import clojure.lang.IPersistentStack;
import clojure.lang.AFn;
import clojure.lang.Ref;
import clojure.lang.PersistentList;
import clojure.lang.PersistentVector;
import clojure.lang.LockingTransaction;

@SuppressWarnings("unchecked")

public class Cell extends Ref implements IAtom
{ public final Ref cellSources;
  public final Ref cellSinks;
  public final Ref cellSetter;

  private static final IFn cljlist = Clojure.var("clojure.core", "list");
  private static final IFn cljconj = Clojure.var("clojure.core", "conj");
  private static final IFn cljmapv = Clojure.var("clojure.core", "mapv");
  private static final IFn cljbind = Clojure.var("clojure.core", "bound-fn*");
  private static final IFn cljeq   = Clojure.var("clojure.core", "=");

  // ref stuff

  public Cell(Object initVal)
  { this(initVal, null); }

  public Cell(Object initVal, IPersistentMap meta)
  { super(initVal, meta);
    cellSources   = new Ref(PersistentVector.EMPTY);
    cellSinks     = new Ref(PersistentVector.EMPTY);
    cellSetter    = new Ref(null); }

  public Object set(Object val)
  { Object f;
    if (((PersistentVector) cellSources.deref()).seq() == null)
    { super.set(val); cellPropagate(); }
    else if ((f = cellSetter.deref()) != null)
    { ((IFn) tryEnsure.invoke(f)).invoke(val); }
    else { sneakyThrow(new Exception("can't set cell: it's a formula")); }
    return deref(); }

  public Object commute(IFn fn, ISeq args)
  { Cell.sneakyThrow(new Exception("can't commute cell: operation not supported"));
    return null; }

  public Object alter(IFn fn, ISeq args)
  { Object f;
    if (((PersistentVector) cellSources.deref()).seq() == null)
    { super.alter(fn, args); cellPropagate(); }
    else if ((f = cellSetter.deref()) != null)
    { args = (args != null ? args : PersistentList.EMPTY);
      ((IFn) tryEnsure.invoke(f)).invoke(fn.applyTo(args.cons(deref()))); }
    else { sneakyThrow(new Exception("can't alter cell: it's a formula")); }
    return deref(); }

  public void setValidator(IFn f)
  { super.setValidator((IFn) cljbind.invoke(f)); }

  synchronized public IRef addWatch(Object key, IFn callback)
  { return super.addWatch(key, (IFn) cljbind.invoke(callback)); }

  public void notifyWatches(Object oldVal, Object newVal)
  { if (! (boolean) cljeq.invoke(oldVal, newVal))
    { super.notifyWatches(oldVal, newVal); } }

  // atom stuff

  public Object reset(Object newVal)
  { Cell c = this;
    try
    { return LockingTransaction.runInTransaction
      ( new AFn()
        { public Object invoke()
          { return c.set(newVal); } } ); }
    catch (Throwable t)
    { sneakyThrow(t); return null; } }

  public boolean compareAndSet(Object oldv, Object newv)
  { Cell c = this;
    try
    { return (boolean) LockingTransaction.runInTransaction
      ( new AFn()
        { public Object invoke()
          { boolean cas = (boolean) cljeq.invoke(oldv, c.deref());
            if (cas) c.set(newv);
            return cas; } } ); }
    catch (Throwable t)
    { sneakyThrow(t); return false; } }

  public Object swap(IFn f)
  { Cell c    = this;
    ISeq args = PersistentList.EMPTY;
    try
    { return LockingTransaction.runInTransaction
      ( new AFn()
        { public Object invoke()
          { return c.alter(f, args); } } ); }
    catch (Throwable t)
    { sneakyThrow(t); return null; } }

  public Object swap(IFn f, Object arg)
  { Cell c    = this;
    ISeq args = (ISeq) PersistentList.creator.invoke(arg);
    try
    { return LockingTransaction.runInTransaction
      ( new AFn()
        { public Object invoke()
          { return c.alter(f, args); } } ); }
    catch (Throwable t)
    { sneakyThrow(t); return null; } }

  public Object swap(IFn f, Object arg1, Object arg2)
  { Cell c    = this;
    ISeq args = (ISeq) PersistentList.creator.invoke(arg1, arg2);
    try
    { return LockingTransaction.runInTransaction
      ( new AFn()
        { public Object invoke()
          { return c.alter(f, args); } } ); }
    catch (Throwable t)
    { sneakyThrow(t); return null; } }

  public Object swap(IFn f, Object x, Object y, ISeq more)
  { Cell c    = this;
    ISeq args = (ISeq) PersistentList.create((List) more.cons(y).cons(x));
    try
    { return LockingTransaction.runInTransaction
      ( new AFn()
        { public Object invoke()
          { return c.alter(f, args); } } ); }
    catch (Throwable t)
    { sneakyThrow(t); return null; } }

  // javelin stuff

  public static Cell input(Object initVal, IPersistentMap meta)
  { return new Cell(initVal, meta); }

  public static Cell formula(Object thunk, PersistentVector sources, Object setter)
  { Object setr = (setter == null ? null : cljbind.invoke(setter));
    Cell   c    = new Cell(null);
    PersistentVector srcs = sources.cons(cljbind.invoke(thunk));
    try
    { LockingTransaction.runInTransaction
      ( new AFn()
        { public Object invoke()
          { c.cellSources.set(srcs);
            c.cellSetter.set(setr);
            for (Object s : srcs)
            { if (s instanceof Cell)
              ((Cell) s).cellSinks.alter(
                cljconj, (ISeq) cljlist.invoke(new WeakReference(c))); }
            c.cellUpdate();
            return null; } } ); }
    catch (Throwable t)
    { sneakyThrow(t); }
    return c; }

  public void cellPropagate()
  { ArrayList<WeakReference> queue =
      new ArrayList((PersistentVector) cellSinks.deref());
    HashSet<WeakReference> seen = new HashSet();
    WeakReference ref;
    Cell c;
    while (!queue.isEmpty())
    { ref = queue.remove(0);
      if (seen.add(ref) && (c = (Cell) ref.get()) != null && c.cellUpdate())
      { queue.addAll((PersistentVector) c.cellSinks.deref()); } } }

  public boolean cellUpdate()
  { PersistentVector srcs;
    srcs        = (PersistentVector) tryEnsure.invoke(cellSources);
    srcs        = (PersistentVector) cljmapv.invoke(tryEnsure, srcs);
    IFn thunk   = (IFn) ((IPersistentStack) srcs).peek();
    Object prev = deref();
    Object curr = super.set(thunk.applyTo(srcs.pop().seq()));
    return !((boolean) cljeq.invoke(prev, curr)); }

  // misc helpers

  public static IFn tryEnsure = new AFn()
  { public Object invoke(Object x)
    { if (x instanceof Ref)
      { ((Ref) x).touch(); return ((Ref) x).deref(); }
      else { return x; } } };

  public static <E extends Throwable> void sneakyThrow(Throwable e) throws E
  { throw (E) e; } }
