/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

/**
 * Whiteflag message definitions utility class
 *
 * </p> This is a non-instantiatable utility class that holds the all message
 * and message field definitions in accordance with the Whiteflag specification.
 * No implementation specific properties and methods are defined by this class.
 */
public class WfMessageDefinitions {

    /* CONSTRUCTORS */

    /** 
     * Prevents the utility class to be instantiated
     */
    private WfMessageDefinitions() {
        throw new IllegalStateException("Cannot instantiate utility class");
    }

    /* PUBLIC STATIC METHODS */

    /**
     * Returns an array with the generic message header fields
     * @return array with {@link WfMessageField}s
     */
    public static final WfMessageField[] getHeaderFields() {
        return createHeaderFields();
    }

    /**
     * Returns an array with the message body fields for the specified message type
     * @param messageCode String with the message code
     * @param offset Integer indicating the starting byte of the first field
     * @return array with {@link WfMessageField}s
     * @throws WfCoreException if the provided message code is invalid
     */
    public static final WfMessageField[] getBodyFields(String messageCode, int offset) throws WfCoreException {
        switch (messageCode) {
            case "P":
            case "D":
            case "E":
            case "S":
            case "Q":
            case "M": return createSignSignalFields(offset);
            case "R": return createResourceFields(offset);
            case "F": return createFreeTextFields(offset);
            case "A": return createAuthFields(offset);
            case "K": return createCryptoFields(offset);
            case "T": return createTextFields(offset);
            default:
                throw new WfCoreException("Invalid message code: " + messageCode);
        }
    }

    /**
     * Returns an array with additional Whiteflag sign/signal message body request fields
     * @param n the number of request objects
     * @return array with {@link WfMessageField}s
     */
    public static final WfMessageField[] getRequestFields(int n, int offset) {
        return createRequestFields(n, offset);
    }

    /* PROTECTED STATIC FIELD DEFINITION METHODS */

    /**
     * Creates an array with Whiteflag message header fields
     * @return array with {@link WfMessageField}s
     */
    protected static final WfMessageField[] createHeaderFields() {
        return new WfMessageField[] {
            new WfMessageField("Prefix", "^WF$", WfMessageField.Encoding.UTF8, 0, 2),
            new WfMessageField("Version", "(?=1)^[A-Z0-9]{1}$", WfMessageField.Encoding.UTF8, 2, 3),
            new WfMessageField("EncryptionIndicator", "(?=0|1|2)^[A-Z0-9]{1}$", WfMessageField.Encoding.UTF8, 3, 4),
            new WfMessageField("DuressIndicator", "^[0-1]{1}$", WfMessageField.Encoding.BIN, 4, 5),
            new WfMessageField("MessageCode", "(?=A|K|T|P|E|S|D|I|M|Q|R|F)^[A-Z]{1}$", WfMessageField.Encoding.UTF8, 5, 6),
            new WfMessageField("ReferenceIndicator", "(?=0|1|2|3|4|5|6|7|8|9)^"+WfMessageField.Encoding.HEX.charset()+"{1}$", WfMessageField.Encoding.HEX, 6, 7),
            new WfMessageField("ReferencedMessage", "^"+WfMessageField.Encoding.HEX.charset()+"{64}$", WfMessageField.Encoding.HEX, 7, 71)
        }; 
    }

    /**
     * Creates an array with Whiteflag authentication message body fields
     * @param offset Integer indicating the starting byte of the first field
     * @return array with {@link WfMessageField}s
     */
    protected static final WfMessageField[] createAuthFields(int offset) {
        return new WfMessageField[] {
            new WfMessageField("VerificationMethod", "(?=1|2)^"+WfMessageField.Encoding.HEX.charset()+"{1}$", WfMessageField.Encoding.HEX, 0 + offset, 1 + offset),
            new WfMessageField("VerificationData", "^"+WfMessageField.Encoding.UTF8.charset()+"*$", WfMessageField.Encoding.UTF8, 1 + offset, -1)
        };
    }

    /**
     * Creates an array with Whiteflag cryptographic message body fields
     * @param offset Integer indicating the starting byte of the first field
     * @return array with {@link WfMessageField}s
     */
    protected static final WfMessageField[] createCryptoFields(int offset) { 
        return new WfMessageField[] {
            new WfMessageField("CryptoDataType", "^"+WfMessageField.Encoding.HEX.charset()+"{2}$", WfMessageField.Encoding.HEX, 0 + offset, 2 + offset),
            new WfMessageField("CryptoData", "^"+WfMessageField.Encoding.HEX.charset()+"*$", WfMessageField.Encoding.HEX, 2 + offset, -1)
        };
    }

    /**
     * Creates an array with Whiteflag free text message body fields
     * @param offset Integer indicating the starting byte of the first field
     * @return array with {@link WfMessageField}s
     */
    protected static final WfMessageField[] createFreeTextFields(int offset) {
        return new WfMessageField[] {
            new WfMessageField("Text", "^"+WfMessageField.Encoding.UTF8.charset()+"*$", WfMessageField.Encoding.UTF8, 0 + offset, -1)
        };
    }

    /**
     * Creates an array with Whiteflag resource message body fields
     * @param offset Integer indicating the starting byte of the first field
     * @return array with {@link WfMessageField}s
     */
    protected static final WfMessageField[] createResourceFields(int offset) { 
        return new WfMessageField[] {
            new WfMessageField("ResourceMethod", "(?=1)^"+WfMessageField.Encoding.HEX.charset()+"{1}$", WfMessageField.Encoding.HEX, 0 + offset, 1 + offset),
            new WfMessageField("ResourceData", "^"+WfMessageField.Encoding.UTF8.charset()+"*$", WfMessageField.Encoding.UTF8, 1 + offset, -1)
        };
    }

    /**
     * Creates an array with Whiteflag test message body fields
     * @param offset Integer indicating the starting byte of the first field
     * @return array with {@link WfMessageField}s
     */
    protected static final WfMessageField[] createTextFields(int offset) {
        return new WfMessageField[] {
            new WfMessageField("PseudoMessageCode", "^[A-Z]{1}$", WfMessageField.Encoding.UTF8, 0 + offset, 1 + offset)
        };
    }

    /**
     * Creates an array with Whiteflag sign/signal message body fields
     * @param offset Integer indicating the starting byte of the first field
     * @return array with {@link WfMessageField}s
     */
    protected static final WfMessageField[] createSignSignalFields(int offset) {
        return new WfMessageField[] {
            new WfMessageField("SubjectCode", "^"+WfMessageField.Encoding.HEX.charset()+"{2}$", WfMessageField.Encoding.HEX, 0 + offset, 2 + offset),
            new WfMessageField("DateTime", "^"+WfMessageField.Encoding.DATETIME.charset()+"$", WfMessageField.Encoding.DATETIME, 2 + offset, 22 + offset),
            new WfMessageField("Duration", "^"+WfMessageField.Encoding.DURATION.charset()+"$", WfMessageField.Encoding.DURATION, 22 + offset, 32 + offset),
            new WfMessageField("ObjectType", "^"+WfMessageField.Encoding.HEX.charset()+"{2}$", WfMessageField.Encoding.HEX, 32 + offset, 34  + offset),
            new WfMessageField("ObjectLatitude", "^"+WfMessageField.Encoding.LAT.charset()+"$", WfMessageField.Encoding.LAT, 34 + offset, 43 + offset),
            new WfMessageField("ObjectLongitude", "^"+WfMessageField.Encoding.LONG.charset()+"$", WfMessageField.Encoding.LONG, 43  + offset, 53 + offset),
            new WfMessageField("ObjectSizeDim1", "^[0-9]{4}$", WfMessageField.Encoding.DEC, 53 + offset, 57 + offset),
            new WfMessageField("ObjectSizeDim2", "^[0-9]{4}$", WfMessageField.Encoding.DEC, 57 + offset, 61 + offset),
            new WfMessageField("ObjectOrientation", "^[0-9]{3}$", WfMessageField.Encoding.DEC, 61 + offset, 64 + offset)
        };
    }
    
    /**
     * Creates an array with additional Whiteflag sign/signal message body request fields
     * @param n the number of request objects
     * @return array with {@link WfMessageField}s
     */
    protected static final WfMessageField[] createRequestFields(int n, int offset) {
        // Determine parameters
        final String FIELDBASENAME = "ObjectType";
        final int OBJECTFIELDSIZE = 2;
        final int QUANTFIELDSIZE = 2;
        final int nFields = (n * 2);

        // Create fields array
        WfMessageField[] fields = new WfMessageField[nFields];
        int startByte = offset;
        for (int i = 0; i < nFields; i += 2) {
            int splitByte = startByte + OBJECTFIELDSIZE;
            int endByte = splitByte + QUANTFIELDSIZE;
            fields[i] = new WfMessageField(FIELDBASENAME + n, "^"+WfMessageField.Encoding.HEX.charset()+"{2}$", WfMessageField.Encoding.HEX, startByte, splitByte);
            fields[i + 1] = new WfMessageField(FIELDBASENAME + n+"Quant", "^[0-9]{2}$", WfMessageField.Encoding.DEC, splitByte, endByte);
            startByte = endByte;                // Next fields starts where this one ends
        }
        return fields;
    }
}
