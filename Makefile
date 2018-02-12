.PHONY: publish

publish: Dockerfile
	docker build -t markhibberd/quote-bot .
	docker push markhibberd/quote-bot
