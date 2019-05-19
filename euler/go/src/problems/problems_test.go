package problems

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"testing"
)

var answers map[int]string

func TestMain(m *testing.M) {
	data, err := ioutil.ReadFile("../../../answers.json")
	if err != nil {
		panic(err)
	}

	err = json.Unmarshal(data, &answers)
	if err != nil {
		panic(err)
	}

	code := m.Run()
	os.Exit(code)
}

func TestProblem01(t *testing.T) {
	answer := Problem01()
	if answer != answers[1] {
		t.Error("Found", answer, "expected", answers[1])
	} else {
		t.Log("Found " + answer + " successfully")
	}
}

func TestProblem02(t *testing.T) {
	answer := Problem02()
	if answer != answers[2] {
		t.Error("Found", answer, "expected", answers[2])
	} else {
		t.Log("Found " + answer + " successfully")
	}
}

func TestProblem03(t *testing.T) {
	answer := Problem03()
	if answer != answers[3] {
		t.Error("Found", answer, "expected", answers[3])
	} else {
		t.Log("Found " + answer + " successfully")
	}
}

func TestProblem04(t *testing.T) {
	answer := Problem04()
	if answer != answers[4] {
		t.Error("Found", answer, "expected", answers[4])
	} else {
		t.Log("Found " + answer + " successfully")
	}
}

func TestProblem05(t *testing.T) {
	answer := Problem05()
	if answer != answers[5] {
		t.Error("Found", answer, "expected", answers[5])
	} else {
		t.Log("Found " + answer + " successfully")
	}
}

func TestProblem06(t *testing.T) {
	answer := Problem06()
	if answer != answers[6] {
		t.Error("Found", answer, "expected", answers[6])
	} else {
		t.Log("Found " + answer + " successfully")
	}
}

func TestProblem07(t *testing.T) {
	answer := Problem07()
	if answer != answers[7] {
		t.Error("Found", answer, "expected", answers[7])
	} else {
		t.Log("Found " + answer + " successfully")
	}
}

func TestProblem08(t *testing.T) {
	answer := Problem08()
	if answer != answers[8] {
		t.Error("Found", answer, "expected", answers[8])
	} else {
		t.Log("Found " + answer + " successfully")
	}
}

func TestProblem09(t *testing.T) {
	answer := Problem09()
	if answer != answers[9] {
		t.Error("Found", answer, "expected", answers[9])
	} else {
		t.Log("Found " + answer + " successfully")
	}
}

func TestProblem10(t *testing.T) {
	answer := Problem10()
	if answer != answers[10] {
		t.Error("Found", answer, "expected", answers[10])
	} else {
		t.Log("Found " + answer + " successfully")
	}
}

func TestProblem11(t *testing.T) {
	answer := Problem11()
	if answer != answers[11] {
		t.Error("Found", answer, "expected", answers[11])
	} else {
		t.Log("Found " + answer + " successfully")
	}
}

func TestProblem12(t *testing.T) {
	answer := Problem12()
	if answer != answers[12] {
		t.Error("Found", answer, "expected", answers[12])
	} else {
		t.Log("Found " + answer + " successfully")
	}
}
