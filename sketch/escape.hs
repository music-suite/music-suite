

>>> {} <> {}
{}

>>> {a} <> {b} 
{a b}

>>> {a} <> {a}
{aa ab}

>>> ({a} <> {a}) <> {a}
{aa ab} <> {a}
{aa ab ac}

>>> {a} <> ({a} <> {a})
{a} <> {aa ab}
{aa ab ac}

