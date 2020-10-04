/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

/**
 * Whiteflag message definitions utility class
 *
 * </p> This is a non-instantiatable utility class that holds all message and
 * message field definitions in accordance with the Whiteflag specification.
 * No implementation specific properties and methods are defined by this class.
 */
public class WfMessageDefinitions {

    /* PROPERTIES: field defintions */

    /* Undefined fields */
    protected static final WfMessageField[] UNDEFINED = new WfMessageField[] {};

    /**
     * Generic message header fields
     */
    protected static final WfMessageField[] headerFields = new WfMessageField[] {
        new WfMessageField("Prefix", "^WF$", WfMessageField.Encoding.UTF8, 0, 2),
        new WfMessageField("Version", "(?=1)^[A-Z0-9]{1}$", WfMessageField.Encoding.UTF8, 2, 3),
        new WfMessageField("EncryptionIndicator", "(?=0|1|2)^[A-Z0-9]{1}$", WfMessageField.Encoding.UTF8, 3, 4),
        new WfMessageField("DuressIndicator", "^[0-1]{1}$", WfMessageField.Encoding.BIN, 4, 5),
        new WfMessageField("MessageCode", "(?=A|K|T|P|E|S|D|I|M|Q|R|F)^[A-Z]{1}$", WfMessageField.Encoding.UTF8, 5, 6),
        new WfMessageField("ReferenceIndicator", "(?=0|1|2|3|4|5|6|7|8|9)^"+WfMessageField.Encoding.HEX.charset()+"{1}$", WfMessageField.Encoding.HEX, 6, 7),
        new WfMessageField("ReferencedMessage", "^"+WfMessageField.Encoding.HEX.charset()+"{64}$", WfMessageField.Encoding.HEX, 7, 71)
    }; 

    /**
     * Authentication message body fields
     */
    protected static final WfMessageField[] authenticationFields = new WfMessageField[] {
        new WfMessageField("VerificationMethod", "(?=1|2)^"+WfMessageField.Encoding.HEX.charset()+"{1}$", WfMessageField.Encoding.HEX, 71, 72),
        new WfMessageField("VerificationData", "^"+WfMessageField.Encoding.UTF8.charset()+"*$", WfMessageField.Encoding.UTF8, 72, -1)
    };

    /**
     * Cryptographic message body fields
     */
    protected static final WfMessageField[] cryptoFields = new WfMessageField[] {
        new WfMessageField("CryptoDataType", "^"+WfMessageField.Encoding.HEX.charset()+"{2}$", WfMessageField.Encoding.HEX, 71, 73),
        new WfMessageField("CryptoData", "^"+WfMessageField.Encoding.HEX.charset()+"*$", WfMessageField.Encoding.HEX, 73, -1)
    };

    /**
     * Free text message body fields
     */
    protected static final WfMessageField[] freetextFields = new WfMessageField[] {
        new WfMessageField("Text", "^"+WfMessageField.Encoding.UTF8.charset()+"*$", WfMessageField.Encoding.UTF8, 71, -1)
    };

    /**
     * Resource message body fields
     */
    protected static final WfMessageField[] resourceFields = new WfMessageField[] {
        new WfMessageField("ResourceMethod", "(?=1)^"+WfMessageField.Encoding.HEX.charset()+"{1}$", WfMessageField.Encoding.HEX, 71, 72),
        new WfMessageField("ResourceData", "^"+WfMessageField.Encoding.UTF8.charset()+"*$", WfMessageField.Encoding.UTF8, 72, -1)
    };

    /**
     * Test message body fields
     */
    protected static final WfMessageField[] testFields = new WfMessageField[] {
        new WfMessageField("PseudoMessageCode", "^[A-Z]{1}$", WfMessageField.Encoding.UTF8, 71, 72)
    };

    /**
     * Sign/signal message body fields
     */
    protected static final WfMessageField[] signsignalFields = new WfMessageField[] {
        new WfMessageField("SubjectCode", "^"+WfMessageField.Encoding.HEX.charset()+"{2}$", WfMessageField.Encoding.HEX, 71, 73),
        new WfMessageField("DateTime", "^"+WfMessageField.Encoding.DATETIME.charset()+"$", WfMessageField.Encoding.DATETIME, 73, 93),
        new WfMessageField("Duration", "^"+WfMessageField.Encoding.DURATION.charset()+"$", WfMessageField.Encoding.DURATION, 93, 103),
        new WfMessageField("ObjectType", "^"+WfMessageField.Encoding.HEX.charset()+"{2}$", WfMessageField.Encoding.HEX, 103, 105),
        new WfMessageField("ObjectLatitude", "^"+WfMessageField.Encoding.LAT.charset()+"$", WfMessageField.Encoding.LAT, 105, 114),
        new WfMessageField("ObjectLongitude", "^"+WfMessageField.Encoding.LONG.charset()+"$", WfMessageField.Encoding.LONG, 114, 124),
        new WfMessageField("ObjectSizeDim1", "^[0-9]{4}$", WfMessageField.Encoding.DEC, 124, 128),
        new WfMessageField("ObjectSizeDim2", "^[0-9]{4}$", WfMessageField.Encoding.DEC, 128, 132),
        new WfMessageField("ObjectOrientation", "^[0-9]{3}$", WfMessageField.Encoding.DEC, 132, 135)
    };

    /* CONSTRUCTORS */

    /** 
     * Prevents the utility class to be instantiated
     */
    private WfMessageDefinitions() {
        throw new IllegalStateException("Cannot instantiate utility class");
    }

    /* METHODS */
    
    /**
     * Creates an array with additional Whiteflag sign/signal message body request fields
     * @param n the number of request objects
     * @return array with {@link WfMessageField}s
     */
    protected static final WfMessageField[] createRequestFields(final int n) {
        // Request field constants
        final String FIELDBASENAME = "ObjectType";
        final int OBJECTFIELDSIZE = 2;
        final int QUANTFIELDSIZE = 2;

        // Determine parameters
        final int nFields = (n * 2);
        int startByte = 0;

        // Create fields array
        WfMessageField[] fields = new WfMessageField[nFields];
        for (int i = 0; i < nFields; i += 2) {
            final int splitByte = startByte + OBJECTFIELDSIZE;
            final int endByte = splitByte + QUANTFIELDSIZE;
            fields[i] = new WfMessageField(FIELDBASENAME + n, "^"+WfMessageField.Encoding.HEX.charset()+"{2}$", WfMessageField.Encoding.HEX, startByte, splitByte);
            fields[i + 1] = new WfMessageField(FIELDBASENAME + n+"Quant", "^[0-9]{2}$", WfMessageField.Encoding.DEC, splitByte, endByte);
            startByte = endByte;                // Next fields starts where this one ends
        }
        return fields;
    }
}
