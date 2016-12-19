#! /bin/bash
cd src
echo "createAssets True \"Assets\" \"Assets.hs\"" | ghci -fno-warn-tabs Utils/CreateAssets.hs 

