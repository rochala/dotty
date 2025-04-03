package dotty.tools.pc.util;

import java.util.List;

public class TraceEvent {
    String name;
    String cat;
    String ph;
    long ts;
    long dur;
    int pid;
    int tid;
    java.util.List<String> args;

    public TraceEvent(
      String name,
      String cat,
      String ph,
      long ts,
      long dur,
      int pid,
      int tid,
      java.util.List<String> args
    ) {
      this.name = name;
      this.cat = cat;
      this.ph = ph;
      this.ts = ts;
      this.dur = dur;
      this.pid = pid;
      this.tid = tid;
      this.args = args;
    }

}
