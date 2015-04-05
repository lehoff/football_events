BEGIN {FS = ","
       format = "\"%s\" => {%d,{%d,%d,%d},{%d,%d},%d},\n"}
           {printf format, $1, $2, $3, $4, $5, $6, $7, $8}
