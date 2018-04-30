<?php

namespace EmacsPHP\Phpactor\Sample;

final class Book
{
    /** @var string */
    private $name;
    /** @var Author[] */
    private $authors;

    /**
     * @param string $name
     * @param Author[] $authors
     */
    public function __construct($name, $authors)
    {
        $this->name = $name;
        $this->authors = $authors;
    }
}
