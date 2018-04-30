<?php

namespace EmacsPHP\Phpactor\Sample;

final class Author
{
    /** @var string */
    private $name;

    /**
     * @param string $name
     */
    public function __construct($name)
    {
        $this->name = $name;
    }

    public function test()
    {
        test();
        foo();
    }
}
