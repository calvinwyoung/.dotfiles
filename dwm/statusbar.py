#!/usr/bin/env python
"""Simple status bar script for DWM."""

import glob
import os
import re
import time
import datetime
import subprocess
import sys

# Define escape characters that change the font color in the statusbar
GREEN = u"\x05"
WHITE = u"\x01"

class BaseWidget(object):
    """The base status bar widget."""
    def get_components(self):
        """Override to return the status bar components (icon and text).

        This method should return a tuple (icon, status_text) where `icon` is
        the character code for the icon that should be shown, and `status_text`
        is the text output for this widget.

        Returns:
            (icon, status_text)
        """
        raise NotImplementedError

    def __unicode__(self):
        icon, status_text = self.get_components()
        return GREEN + icon + WHITE + status_text

class ClockWidget(BaseWidget):
    """A basic clock widget that shows the current date and time."""
    def get_components(self):
        return (u"\uE015", datetime.datetime.now().strftime("%a %b %d %r"))

class MemUsageWidget(BaseWidget):
    """Displays memory usage information in megabytes and as a percent."""
    def get_components(self):
        with open("/proc/meminfo") as f:
            # The first 4 lines of mem_info give us the info we want
            mem_total_kb = int(f.readline().split()[1])
            mem_free_kb = int(f.readline().split()[1])
            buffers_kb = int(f.readline().split()[1])
            cached_kb = int(f.readline().split()[1])

        mem_used_kb = mem_total_kb - mem_free_kb - buffers_kb - cached_kb
        mem_used_gb = mem_used_kb / (1024.0 ** 2)
        mem_used_perc = 100 * mem_used_kb / mem_total_kb

        status_text = "%d%% (%.2fG)" % (mem_used_perc, mem_used_gb)
        return (u"\uE020", status_text)

class CPUTempWidget(BaseWidget):
    """Displays the temperatures of the CPU cores."""
    def __init__(self):
        self.coretemp_paths = glob.glob(
            "/sys/bus/platform/drivers/coretemp/coretemp.*")

    def get_components(self):
        if not self.coretemp_paths:
            raise RuntimeError("No coretemp paths found.")

        temps = []
        for path in self.coretemp_paths:
            with open(os.path.join(path, "temp1_input")) as f:
                temps.append(int(f.read()) / 1000)

        status_text = " / ".join("%dC" % t for t in temps)
        return (u"\uE01c", status_text)

class CPUUsageWidget(BaseWidget):
    """Displays the percent utilization per CPU core over the last interval."""
    def __init__(self):
        # The used/total CPU time for the previous and current iteration. The
        # length of each list is equal to the number of CPU cores. These lists
        # are updated at the beginning of each interval to compute the percent
        # utilization during the previous interval.
        self.prev_used, self.prev_total = [], []
        self.cur_used, self.cur_total = [], []

        # Execute one iteration of `get_components()` immediately so we have
        # both `prev_` and `cur_` values in the next iteration.
        self.get_components()

    def get_components(self):
        self.prev_used, self.prev_total = self.cur_used, self.cur_total
        self.cur_used, self.cur_total = [], []
        with open("/proc/stat") as f:
            for line in f:
                if not line.startswith("cpu"):
                    break
                elif re.match("cpu\d", line):
                    words = line.split()
                    user_time = int(words[1])
                    nice_time = int(words[2])
                    system_time = int(words[3])
                    idle_time = int(words[4])

                    self.cur_used.append(user_time + nice_time + system_time)
                    self.cur_total.append(self.cur_used[-1] + idle_time)

        delta_used = [(c - p) for c, p in zip(self.cur_used, self.prev_used)]
        delta_total = [(c - p) for c, p in zip(self.cur_total, self.prev_total)]

        percent_used = []
        for used, total in zip(delta_used, delta_total):
            percent_used.append(100 * used / total if total > 0 else 0)

        status_text = " / ".join("%2d%%" % percent for percent in percent_used)
        return (u"\uE026", status_text)

class NetSpeedWidget(BaseWidget):
    """Displays the current download and upload speed in kb/s."""
    def __init__(self):
        # The number of downloaded and uploaded bytes at the beginning of the
        # previous and current iterations. These numbers are updated at the
        # beginning of each iteration to compute the download/upload speed
        # during the previous iteration.
        self.prev_down, self.prev_up = None, None
        self.cur_down, self.cur_up = None, None

        # Execute one iteration of `get_components()` immediately so we have
        # both `prev_` and `cur_` values in the next iteration.
        self.get_components()

    def get_components(self):
        # Get the network interface currently associated with the default
        # route. This is done by reading /proc/net/route, and finding the
        # interface associated with the default gateway, which is represented as
        # a string of eight 0s.
        net_iface = None
        with open("/proc/net/route") as f:
            match = re.search("^(\S+)\s+0{8}", f.read(), flags=re.MULTILINE)
            net_iface = match.group(1)

        self.prev_down, self.prev_up = self.cur_down, self.cur_up

        # Then read the received/transferred bytes on the network interface
        with open("/sys/class/net/%s/statistics/rx_bytes" % net_iface) as f:
            self.cur_down = int(f.read())

        with open("/sys/class/net/%s/statistics/tx_bytes" % net_iface) as f:
            self.cur_up = int(f.read())

        if self.prev_down is None or self.prev_up is None:
            speed_down, speed_up = 0, 0
        else:
            speed_down = (self.cur_down - self.prev_down) / 1024.0
            speed_up = (self.cur_up - self.prev_up) / 1024.0

        icons = (u"\uE061", u"\uE060")
        status_texts = (u"%4.1fk/s" % speed_down, "%4.1fk/s" % speed_up)
        return (icons, status_texts)

    def __unicode__(self):
        """Override to show both down and up icons next to their speeds."""
        icons, status_texts = self.get_components()

        return u"%s%s%s%s %s%s%s%s" % (GREEN, icons[0], WHITE, status_texts[0],
                                       GREEN, icons[1], WHITE, status_texts[1])

class WirelessInfoWidget(BaseWidget):
    """Displays the essid and signal strength of the wireless network."""
    def get_components(self):
        p = subprocess.Popen(["iwgetid"], stdout=subprocess.PIPE)

        match = re.match("^(\w+)\s+ESSID:\"(.*)\"$", p.communicate()[0])
        net_iface, essid = match.groups()

        with open("/sys/class/net/%s/wireless/link" % net_iface) as f:
            signal_strength = int(f.read())

        status_text = "%s %d%%" % (essid, signal_strength)
        return (u"\uE048", status_text)

def main():
    """Main loop."""
    widgets = [
        WirelessInfoWidget(),
        NetSpeedWidget(),
        CPUUsageWidget(),
        CPUTempWidget(),
        MemUsageWidget(),
        ClockWidget()
    ]

    while True:
        results = []
        for widget in widgets:
            try:
                widget_str = unicode(widget)
            except Exception, e:
                pass
            else:
                results.append(widget_str)

        # Flush the output buffer after each write so another process can read
        # it line-by-line.
        print " |".join(results).encode("utf-8")
        sys.stdout.flush()

        time.sleep(1)

if __name__ == "__main__":
    main()
