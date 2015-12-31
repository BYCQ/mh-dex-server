#!/bin/bash

# http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

let icon_id=-1

echo '' > ${DIR}/../data/items/item-icon.lisp

for f in $(ls ${DIR}/../data/items/Itm_????.png); do
    true_name=$(readlink -f ${f})

    let found=-1
    for saved in $(ls ${DIR}/../data/items/icon_*.png); do
        diff ${true_name} ${saved} >> /dev/null
        if [ "$?" = "0" ]; then
            found=$(echo ${saved} | sed 's/^.*icon_\([0-9]\+\).png$/\1/');
            break;
        fi
    done

    item_id=$(echo ${true_name} | sed 's/^.*Itm_0*\([1-9][0-9]*\).png$/\1/');
    let item_id=$((item_id-1))

    if (( "${found}" > "-1" )); then
        # Found a match in the saved icons.
        echo "(${item_id} ${found})" >> ${DIR}/../data/items/item-icon.lisp
    else
        # No match.
        let icon_id=$((icon_id+1));
        cp ${true_name} ${DIR}/../data/items/icon_${icon_id}.png
        echo "(${item_id} ${icon_id})" >> ${DIR}/../data/items/item-icon.lisp
    fi

    echo "[ok] Processed ${true_name}."
done
    
