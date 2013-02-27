import math

"""
Try this code by:
simulate(10, 100)
"""


def expfit(backups, now):
    exponent = math.log(now) / math.log(len(backups))
    
    def exp_deviation(backup, backupnr):
        return abs(now - backup - backupnr ** exponent)
    return sum(exp_deviation(b, bnr) for bnr, b in enumerate(reversed(backups)))


def tryall(backups):
    remove = min((expfit(backups[:i] + backups[i + 1:], backups[-1]), i) for i in xrange(len(backups)))[1]
    return backups[:remove] + backups[remove + 1:]


def simulate(nrbackups, targetnow):
    backups = range(nrbackups)
    for now in xrange(nrbackups, targetnow + 1):
        backups = tryall(backups + [now])
    return backups
