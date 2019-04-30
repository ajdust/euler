
def hammCount(left: str, right: str):
    count = 0
    for l, r in zip(left, right):
        if l != r:
            count += 1
    return count

def hamm(stream):
    line1 = next(stream)
    line2 = next(stream)
    return hammCount(line1, line2)
