from bintrees import RBTree
from netaddr import IPNetwork, IPAddress
from collections import namedtuple

## predicate is True/False just for example, in real
## life this should be matching against some additional
## rulest, such as CLI/CLD regex, etc.
Rule = namedtuple('Rule', 'net pred handler')

class IPSearcher():
    t = None
    def __init__(self, rules):
        t = RBTree()
        ## to avoid exceptions during LE lookup
        t.insert(0, [])
        nets = []
        for rule in rules:
            net = IPNetwork(rule.net)
            t.insert(net.first, [])
            t.insert(net.last+1, [])
        for prio, rule in enumerate(rules):
            net = IPNetwork(rule.net)
            for k,v in t.iter_items(net.first, net.last+1):
                v.append((prio, rule))
        self.t = t

    def match_rules(self, ip):
        ip = IPAddress(ip)._value
        for prio, rule in sorted(self.t.floor_item(ip)[1]):
            if rule.pred:
                return rule.handler
        return None

rules = [
    Rule('0.0.0.1', True, 'Auth #1'),
    Rule('1.1.1.1/8', False, 'Auth with 2'),
    Rule('1.1.1.1', True, 'Auth with 21'),
    Rule('0.0.0.0/0', True, 'Auth with 1'),
    Rule('255.1.2.3/28', True, 'Auth with 7')]

test_ips = ['1.1.1.1', '1.1.1.2', '0.1.1.1', '255.244.255.2']


def etalon_get_rule(ip):
    for rule in rules:
        if IPAddress(ip) in IPNetwork(rule.net) and rule.pred:
            return rule.handler
    return None

s = IPSearcher(rules)

for ip in test_ips:
	res = s.match_rules(ip)
	et_res = etalon_get_rule(ip)
	assert res == et_res
