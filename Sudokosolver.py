# build a backtracking function that finds the combinations of 
# 9 numbers where  every number appears only once
def reject(partial_candidate):
    # recect a partial solution that can already be seen 
    # not to survive
    # here we immediately reject all list that are not unique
    return len(partial_candidate) != len(set(partial_candidate))

def accept(candidate):
    # only accept a complete solution
    return len(candidate)==3

def bt(sols,pc):
    if reject(pc):
        return sols
    else:
        if accept(pc):
            return sols+[pc]
        else:
            new_pcs=[pc+[i] for i in range(3)]
            return sum([bt(sols,pc) for pc in new_pcs],[])
