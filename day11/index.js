

const a = "[]"

let count = 0
let openers = []

const isCloser = (a, b) => {
	if (a === '(' && b === ')') {
		return true;
	}

	if (a === '[' && b === ']') {
		return true;
	}

	if (a === '<' && b === '>') {
		return true;
	}

	if (a === '{' && b === '}') {
		return true;
	}

	return false
}

for (let b of a) {
	if("([{<".includes(b)) {
		openers = [b, ...openers]
		continue
	}

	[lastOpener, ...rest ] = openers
	if (isCloser(lastOpener, b)) {
		openers = rest
		continue
	}

	console.log("invalid")
	return false
}