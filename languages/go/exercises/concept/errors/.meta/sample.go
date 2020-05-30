package deepthought

import (
	"errors"
	"fmt"
)

const answer = 42
var ErrCalculation = errors.New("calculation error:")

// AnswerToLife checks if your number is the answer to life
func AnswerToLife(number int) (bool, error) {
	if number != answer {
		return false, fmt.Errorf("%s wrong answer to life: %s", ErrCalculation.Error(), number)
	}
	return true, nil
}
