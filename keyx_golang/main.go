package main

import (
	"fmt"
	"encoding/hex"
	"github.com/hashicorp/vault/shamir"
	"os"
	"log"
	"strconv"
)

func usage(l *log.Logger) {
	usageSplit(l)
	usageCombine(l)
}

func usageSplit(l *log.Logger) {
	l.Printf("usage: %s split Parts(Int) Threshold(Int) Secret(Hex)", os.Args[0])
}

func usageCombine(l *log.Logger) {
	l.Printf("usage: %s combine Share1(Hex) Share2(Hex) ... ShareK(Hex)", os.Args[0])
}

func split(K, N, Secret string) (err error) {
	secret, err := hex.DecodeString(Secret)
	if err != nil {
		return
	}
	k, err := strconv.Atoi(K)
	if err != nil {
		return
	}
	n, err := strconv.Atoi(N)
	if err != nil {
		return
	}
	
	out, err := shamir.Split(secret, k, n)
	if err != nil {
		return
	}
	for _, share := range out {
		fmt.Printf("%x\n", share)
	
	}
	return nil
}

func combine(Parts []string) (err error) {
	parts := make([][]byte, len(Parts))
	for idx := range Parts {
		if parts[idx], err = hex.DecodeString(Parts[idx]); err != nil {
			return err
		}
	}
	recomb, err := shamir.Combine(parts)
	if err != nil {
		return
	}
	fmt.Printf("%x\n", recomb)
	return nil
}
	
func main() {
	l := log.New(os.Stderr, "", 0)
	
	if len(os.Args) < 4 {
		usage(l)
		os.Exit(2)
	}
	switch os.Args[1] {

	case "split":
		if len(os.Args) != 5 {
			usageSplit(l)
			os.Exit(2)
		}
		if err := split(os.Args[2], os.Args[3], os.Args[4]); err != nil {
			l.Fatal(err)
		}
	case "combine":
		if len(os.Args) < 4 {
			usageCombine(l)
			os.Exit(2)
		}
		if err := combine(os.Args[2:]); err != nil {
			l.Fatal(err)
		}
	default:
		usage(l)
		os.Exit(2)
	}
}
